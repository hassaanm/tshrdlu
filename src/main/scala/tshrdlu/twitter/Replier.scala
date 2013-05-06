package tshrdlu.twitter

import akka.actor._
import twitter4j._

/**
 * An actor that constructs replies to a given status.
 */
trait BaseReplier extends Actor with ActorLogging {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import scala.concurrent.Future
  import akka.pattern.pipe

  def receive = {
    case ReplyToStatus(status) => 
      val replyName = status.getUser.getScreenName
      val candidatesFuture = getReplies(status, 138-replyName.length)
      candidatesFuture.map { candidates =>
        candidates.toSet.headOption.map({ replyText:String => 
          val reply = "@" + replyName + " " + replyText
          log.info("Candidate reply: " + reply)
          new StatusUpdate(reply).inReplyToStatusId(status.getId)
        })
      } pipeTo sender
  }

  def getReplies(status: Status, maxLength: Int): Future[Seq[String]]

}

/**
 * Given a tweet that was directed to the bot, return a reply tweet that contains
 * information about one company that was mentioned. If multiple companies were
 * mentioned or no companies were mentioned, the bot instead asks for clarification.
 * A special syntax is available to update our inverted index and add new company
 * information. Special syntax: "UPDATE (SYMBOL): Company name"
 */
class BusinessReplier extends BaseReplier {
    import Bot._
    import TwitterRegex._
    import tshrdlu.util.{CompanyData, English, Polarity, AlphaNumericTokenizer, Resource}
    
    import context.dispatcher
    import scala.concurrent.duration._
    import scala.concurrent.Future
    import akka.pattern._
    import akka.util._
    import java.util.Random
    import nak.NakContext._
    import nak.core._

    implicit val timeout = Timeout(10 seconds)
    lazy val polarity = new Polarity()
    // File containing Bit.ly API key and username
    lazy val bitlyFile = scala.io.Source.fromFile("bitly.properties").getLines.toList
    lazy val bitlyApiKey : String = bitlyFile(0)
    lazy val bitlyLogin : String = bitlyFile(1)
    // File containing New York Times API key
    lazy val nytimesFile = scala.io.Source.fromFile("nytimes.properties").getLines.toList
    lazy val nytimesApiKey : String = nytimesFile(0)
    // Regex to match our special syntax
    lazy val updateRegex = """UPDATE \(([A-Z]+)\): (.+[A-Za-z]+.+)""".r
    // Text to tweet when our bot is unsure which company was mentioned
    lazy val confusedQuery = "Can you provide the stock symbol?"
    // Tweets for when all else fails
    // Should only be tweeted if status only contains stopwords
    lazy val defaultResponses = List(
      "I'm not sure.",
      "I'm confused.",
      "I need some help.",
      "Please clarify.",
      "I don't know what you mean.",
      "Did you ask about a company?",
      "I'm not sure which company you asked about.",
      "I can only give information about companies in the NYSE and NASDAQ.",
      "I couldn't parse that.",
      "Can you be more specific?",
      "I'm all business."
      )
    lazy val rand = new Random();
    lazy val classifier = loadClassifier[LiblinearClassifier with FeaturizedClassifier[String, String]]("src/main/resources/BusinessArticle.classifier")
    
    /**
     * Given a tweet that was directed to the bot, return a reply tweet.
     * @param status the status to be replied to
     * @param maxLength the maximum length the tweet can be
     * @return A Future[Seq[String]] containing the reply tweet.
     */
    def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
        log.info("I'm all business.")
        val text = stripLeadMention(status.getText)
        if(text.matches(updateRegex.toString)){
            // We matched the special syntax that tells us to update our inverted index
            val updateRegex(sym, comp) = text
            val updates = CompanyData.updateIndex(sym, comp)
            log.info("Updating index" + updates.mkString(", "))
            return getBusinessTweet(sym, comp, maxLength)
        }
        val importantWords = AlphaNumericTokenizer(text)
                .filterNot(English.stopwords.contains(_))
        log.info("Searching for companies with the words: " + importantWords.mkString(" "))
        val extractedCompanies = extractCompanySymbols(importantWords)
        val successful = extractedCompanies._1
        if (successful) {
            // We extracted exactly one company name
            val companies = extractedCompanies._2
            log.info("Companies: " + companies.mkString(","))
            val symbol = companies.head
            val compName = CompanyData.symToComp.getOrElse(symbol, "")

            return getBusinessTweet(symbol, compName, maxLength)
        }
        else {
            // We found multiple or no companies
            val confusingWords = extractedCompanies._2
            if (confusingWords.size == 0) {
                if(importantWords.size == 0) {
                    val random_index = rand.nextInt(defaultResponses.size);
                    val randomResponse = defaultResponses(random_index);
                    return Future(Seq(randomResponse + " " + confusedQuery))
                }
                return getConfusedTweet("I couldn't find a company with the word", importantWords, maxLength)
            }
            return getConfusedTweet("I found multiple companies with the word", confusingWords, maxLength)
        }
    }

    /**
     * Get a tweet containing information about the desired company.
     * @param symbol the stock symbol of the company
     * @param compName the name of the company
     * @param maxLength the maximum length the tweet can be
     * @return a Future[Seq[String]] that contains the tweet
     */
    def getBusinessTweet(symbol: String, compName: String, maxLength: Int): Future[Seq[String]] = {
        val statusList =
            List(symbol, compName)
            .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

        val statusesFuture: Future[Seq[Status]] = Future.sequence(statusList).map(_.toSeq.flatten)

        statusesFuture
            .map(status => extractText(status, symbol, compName, maxLength))
            .map(_.filter(_.length <= maxLength))
    }

    /**
     * Get a tweet indicating that the bot is confused.
     * @param beginningText the text to begin the tweet with
     * @param confusingWords a list of words that confused the bot
     * @param maxLength the maximum length the tweet can be
     * @return a Future[Seq[String]] that contains the tweet
     */
    def getConfusedTweet(beginningText: String, confusingWords: List[String], maxLength: Int): Future[Seq[String]] = {
        val s = if(confusingWords.size > 1) "s" else ""
        val actualBegText = beginningText + s + " \""
        val actualEndText = ".\" " + confusedQuery
        val maxLen = maxLength - (actualBegText + actualEndText).size
        val confusingWordsText = confusingWords.mkString(", ")
        // Safety buffer of one character.
        val actualConfWordsText = if(confusingWordsText.size < maxLen) confusingWordsText
            else (confusingWordsText.substring(0, maxLen - 4) + "...")
        Future(Seq(actualBegText + actualConfWordsText + actualEndText))
    }

    /**
     * Extracts a list of possible stock symbols or confusing words from a list of words.
     * @param words A list of words that might be the names of companies
     * @return The first element of the tuple indicated whether extraction was successful
     *         If there is an obvious company mentioned, returns a list of length 1 with the company symbol
     *         If there are multiple options, returns a list of the words that are most confusing
     *         If there are no companies found, returns an empty list
     */
    def extractCompanySymbols(words: List[String]): (Boolean, List[String]) = {
        val companyMentions = (for(word <- words) yield CompanyData.invertedIndex.get(word))
            .flatten
            .flatten
            .groupBy(identity)
            .mapValues(x => x.length)
            .toList
            .sortBy(-_._2)
        if(companyMentions.size == 0)
            return (false, List[String]())
        val mostMentions = companyMentions.head._2
        val topMentionedCompanies = companyMentions.filter(_._2 == mostMentions).map(_._1)
        log.info("Mentions: " + companyMentions.mkString(", "))
        log.info("Most mentions: " + topMentionedCompanies.mkString(", "))
        if(topMentionedCompanies.length == 1 || topMentionedCompanies.length == 0)
            return (topMentionedCompanies.length == 1, topMentionedCompanies)
        // We are confused, determine which words confused us
        val allCompanies = AlphaNumericTokenizer(topMentionedCompanies
            .map(x => CompanyData.symToComp.getOrElse(x, "") + " " + x)
            .mkString(" "))
        log.info(allCompanies.mkString(", "))
        val confusingWords = words.filter(allCompanies.contains)
        log.info("Confusing words: " + confusingWords.mkString(", "))
        (false, confusingWords)
    }

    /**
     * Measures the importance of a word by dividing the total number of words in
     * the inverted index by the number of companies with that word in their name.
     * Words that occur less frequently will be more important.
     * @param word the word to be measured
     * @return the importance of the word
     */
    def wordImportance(word: String): Double = {
        val numOccurrences = CompanyData.invertedIndex.getOrElse(word, List[String]()).size.toDouble
        if(numOccurrences == 0)
            return 0
        CompanyData.invertedIndex.size / numOccurrences
    }

    /**
     * Analyzes tweets about a company, and stock price info from Yahoo! Finance to
     * generate a tweet containing information about a company's stock.
     * @param statusList a sequence containing tweets about a company
     * @param symbol the symbol of the company
     * @param company the name of the company
     * @param maxLength the maximum length the tweet can be
     * @return A sequence containing a tweet with info about the desired company.
     */
    def extractText(statusList: Seq[Status], symbol: String, company: String, maxLength: Int): Seq[String] = {
        val useableTweets = statusList
            .map(_.getText)
            .map {
                case StripMentionsRE(rest) => rest
                case x => x
            }
            .filterNot(_.contains('@'))
            .filterNot(_.contains('/'))
            .filter(English.isEnglish)
            .filter(English.isSafe)

        val sentimentVals = for (tweet <- useableTweets) yield getSentiment(tweet)
        val avgSentiment = sentimentVals.sum / sentimentVals.length
        val yahooFixedSymbol = symbol.replaceAll("\\^", "-P")
        val (price, outlook) = symbolInfo(yahooFixedSymbol)

        // use articles to determine price outlook
        def maxLabelPpa = maxLabel(classifier.labels) _
        val articles = getArticles(company)
        val predictions = for(text <- articles) yield maxLabelPpa(classifier.evalRaw(text))
        val priceOutlook = "Outlook: " + (if (predictions.length > 0)
                        predictions.groupBy(x=>x).mapValues(x=>x.length).toSeq.sortBy(-_._2).head._1
                    else
                         (if (outlook > 0.7) "Good" else if (outlook < 0.3) "Bad" else "OK")) + ", "

        val symbolText = " (" + symbol + "), "
        val lastPrice = "Price: " + price + ", "
        val sentiment = "Opinion: " + (if (avgSentiment > 0.01) "Good" else if (avgSentiment < -0.01) "Bad" else "OK") + ", "
        val yahooLink = "Info: " + shortenURL("""http://finance.yahoo.com/q?s=""" + yahooFixedSymbol)
        
        log.info("Sentiment: " + avgSentiment)

        val infoText = symbolText + lastPrice + priceOutlook + sentiment + yahooLink
        val maxLen = maxLength - (infoText).size
        // Safety buffer of one character.
        val companyText = if(company.size < maxLen) company
            else (company.substring(0, maxLen - 4) + "...")

        val response = companyText + infoText
        log.info("SentimentReplier responding with: " + response)
        Seq(response)
    }

    /**
     * Estimates the sentiment of a text. Iterates through the words with a sliding
     * window two wide. If the first word in the window is in our negationsWords set,
     * the polarity of the second word is inversed.
     * @param text the text
     * @return The estimated sentiment of the text.
     */
    def getSentiment(text: String): Double = {
        val words = AlphaNumericTokenizer(text)
        var numPos = if(polarity.posWords.contains(words(0))) 1 else 0
        var numNeg = if(polarity.negWords.contains(words(0))) 1 else 0
        for(wordSet <- words.sliding(2)){
            val negate = English.negationWords.contains(wordSet(0))
            if (polarity.posWords.contains(wordSet(1))){
                if(negate) numNeg += 1
                else numPos += 1
            }
            if (polarity.negWords.contains(wordSet(1))){
                if(negate) numPos += 1
                else numNeg += 1
            }
        }
        val sentiment = if (numPos != 0) { numPos.toDouble / (numPos + numNeg) } else 0
        log.info("Sentiment of \"" + text + "\" = " + sentiment)
        sentiment
    }
    
    def stockInfo(jsonData: Option[Any], key: String): String = {
        jsonData match { 
            case Some(m: Map[String, Any]) => m("query") match {
                case n: Map[String, Any] => n("results") match {
                    case o: Map[String, Any] => o("quote") match {
                        case p: Map[String, Any] => p(key) match {
                            case s: String => s
                            case null => "0.0"
                        }
                    }
                }
            }
        }
    }

    /**
     * Given a stock symbol, return the current price and our prediction of
     * the future outlook for the stock.
     * @param symbol the stock symbol
     * @return a tuple: (current price, outlook prediction)
     */
    def symbolInfo(symbol: String): (Double, Double) = {
        val url = """http://query.yahooapis.com/v1/public/yql?env=http%3A%2F%2Fdatatables.org%2Falltables.env&format=json&q=select%20*%20from%20yahoo.finance.quote%20where%20symbol%20in%20(%22""" + symbol + """%22)"""
        log.info(symbol)
        log.info(url)
        val json = scala.io.Source.fromURL(url).mkString
        val jsonData = scala.util.parsing.json.JSON.parseFull(json)
        log.info(jsonData.toString)
        val price = stockInfo(jsonData, "LastTradePriceOnly").toDouble
        val yearLow = stockInfo(jsonData, "YearLow").toDouble
        val yearHigh = stockInfo(jsonData, "YearHigh").toDouble
log.info("13")
        val range = yearHigh - yearLow
        val outlook = (price - yearLow) / range

        (price, outlook)
    }

    /**
     * Given a url, returns the result of shortening the url with Bit.ly.
     * @param longUrl the url to be shortened
     * @return a new Bit.ly url pointing to the same web address as longUrl
     */
    def shortenURL(longUrl: String): String = {
        val link ="http://api.bit.ly/v3/shorten?format=txt&login="+bitlyLogin+"&apiKey="+bitlyApiKey+"&longUrl="+longUrl
        try {
            val shortUrl = scala.io.Source.fromURL(link).mkString
            shortUrl.trim
        } catch  {
            case e: Exception => "http://yhoo.it/12GRbyV"
        }
    }

    def articleCount(jsonData: Option[Any]): Int = {
        jsonData match {
            case Some(m: Map[String, Any]) => m("response") match {
                case d: Map[String, Any] => d("docs") match {
                    case l: List[Map[String, Any]] => l.length
                }
            }
        }
    }

    def articleTitle(jsonData: Option[Any], index: Int): String = {
        jsonData match {
            case Some(m: Map[String, Any]) => m("response") match {
                case d: Map[String, Any] => d("docs") match {
                    case l: List[Map[String, Any]] => l(index) match {
                        case a: Map[String, Any] => a("headline") match {
                            case h: Map[String, String] => h("main")
                        }
                    }
                }
            }
        }
    }

    def articleInfo(jsonData: Option[Any], index: Int, key: String): String = {
        jsonData match {
            case Some(m: Map[String, Any]) => m("response") match {
                case d: Map[String, Any] => d("docs") match {
                    case l: List[Map[String, Any]] => l(index) match {
                        case a: Map[String, Any] => a(key) match {
                            case s: String => s
                            case null => "" 
                        }
                    }
                }
            }
        }
    }

    def isArticleBusiness(jsonData: Option[Any], index: Int): Boolean = {
        val section = jsonData match {
            case Some(m: Map[String, Any]) => m("response") match {
                case d: Map[String, Any] => d("docs") match {
                    case l: List[Map[String, Any]] => l(index) match {
                        case a: Map[String, Any] => a("section_name") match {
                            case s: String => s
                            case null => "" 
                        }
                    }
                }
            }
        }
        (section == "Business Day" || section == "Technology" || section == "Your Money")
    }

    def getArticles(company: String): Seq[String] = {
        val url = "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=" + company.replaceAll(" ", "+") + "&sort=newest&api-key=" + nytimesApiKey

        try {
            val json = scala.io.Source.fromURL(url).mkString
            val jsonData = scala.util.parsing.json.JSON.parseFull(json)
            val articles = (for (i <- 0 to (articleCount(jsonData) - 1)) yield {
                                if (isArticleBusiness(jsonData, i)) {
                                    articleTitle(jsonData, i) +
                                    articleInfo(jsonData, i, "lead_paragraph") +
                                    articleInfo(jsonData, i, "abstract")
                                }
                                else {
                                    ""
                                }
                            }).filterNot(List("").contains)
            articles
        } catch {
            case e: Exception => List()
        }
    }
}

/**
 * An actor that constructs replies to a given status.
 */
class SynonymReplier extends BaseReplier {
  import Bot._ 
  import tshrdlu.util.English.synonymize
  import TwitterRegex._

  import context.dispatcher
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply synonym")
    val text = stripLeadMention(status.getText).toLowerCase
    val synTexts = (0 until 10).map(_ => Future(synonymize(text))) 
    Future.sequence(synTexts).map(_.filter(_.length <= maxLength))
  }

}

/**
 * An actor that constructs replies to a given status.
 * For best results, tweet at me something related to one of the 
 * topics from the "20 Newsgroups" data
 * e.g. Religion, baseball, atheism, windows, hockey, mideast, pc hardware
 */
class TopicModelReplier extends BaseReplier {
  import Bot._ 
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  val modeler = new TopicModeler("minTopicKeys.txt")

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply via topic models")
    val text = stripLeadMention(status.getText).toLowerCase
    val statusTopicList = SimpleTokenizer(text)
				.filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten

	val topicWords:List[String] = statusTopicList.map(topic => 
		modeler.topicWordsMap.getOrElse(topic,Set(" "))).take(4).flatten

	val statusQueryList :List[String] = topicWords
				.filter(_.length > 4)
                .filter(_.length < 11)
	        	.sortBy(- _.length)
				.distinct
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = 
		if(statusQueryList.length <1) {
			SimpleTokenizer(text)
				.filter(_.length > 3)
				.filter(_.length < 10)
				.filterNot(_.contains('/'))
				.filter(tshrdlu.util.English.isSafe)
				.sortBy(- _.length)
				.take(3) 
				.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}
		else { statusQueryList
    			.map(w => (context.parent ? 
					SearchTwitter(new Query(w))).mapTo[Seq[Status]])}

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      	Future.sequence(statusSeqFutures).map(_.flatten)

	statusesFuture.map{x => extractText(x, statusTopicList.toSet)}
  }

  /**
   * Go through the list of tweets, gets "proper" tweets, determines
   * topic distribution vectors of said tweets, calculates similarities
   * between original tweet and candidate tweets
   * Returns most similar tweeet
   */
  def extractText(statusList: Seq[Status], statusTopics: Set[String]) = {
    val useableTweets = statusList
      .map(_.getText)
      .map {
			case StripMentionsRE(rest) => rest
			case x => x
      }
      .filterNot(_.contains('@'))
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isEnglish)
      .filter(tshrdlu.util.English.isSafe)

    //Use topic model to select response
    val topicDistributions = for ( tweet <- useableTweets) yield {
    			SimpleTokenizer(tweet).filter(_.length > 4)
				.toSet
				.take(3)
				.toList
				.flatMap(w => modeler.wordTopicsMap.get(w))
				.flatten}
    
    val topicSimilarity = topicDistributions.map(ids => 
		ids.toSet.intersect(statusTopics).size * {
			if(statusTopics.size -ids.toSet.size ==0 ) 1 
			else (1/math.abs(statusTopics.size - ids.toSet.size)).toDouble})
    
    val topTweet = topicSimilarity.toList.zip(useableTweets).maxBy(_._1)._2

    List(topTweet)
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }
}

/**
 * An actor that constructs replies to a given status.
 */
class StreamReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status.
   */
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)
    val statusSeqFutures: Seq[Future[Seq[Status]]] = SimpleTokenizer(text)
    .filter(_.length > 3)
    .filter(_.length < 10)
    .filterNot(_.contains('/'))
    .filter(tshrdlu.util.English.isSafe)
    .sortBy(- _.length)
    .take(3)
    .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[Status]] =
      Future.sequence(statusSeqFutures).map(_.flatten)

    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.map(_.flatMap(getText).filter(_.length <= maxLength))
  }


  /**
   * Go through the list of Statuses, filter out the non-English ones and
   * any that contain (known) vulgar terms, strip mentions from the front,
   * filter any that have remaining mentions or links, and then return the
   * head of the set, if it exists.
   */
  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

}


/**
 * An actor that constructs replies to a given status based on synonyms.
 */
class SynonymStreamReplier extends StreamReplier {
  import Bot._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  import tshrdlu.util.English._
  import TwitterRegex._
  override implicit val timeout = Timeout(10000)


  override def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do synonym search")
    val text = stripLeadMention(status.getText).toLowerCase

    // Get two words from the tweet, and get up to 5 synonyms each (including the word itself).
    // Matched tweets must contain one synonym of each of the two words.

    val query:String = SimpleTokenizer(text)
      .filter(_.length > 3)
      .filter(_.length < 10)
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isSafe)
      .filterNot(tshrdlu.util.English.stopwords(_))
      .take(2).toList
      .map(w => synonymize(w, 5))
      .map(x=>x.mkString(" OR ")).map(x=>"("+x+")").mkString(" AND ")

    log.info("searched for: " + query)

    val futureStatuses = (context.parent ? SearchTwitter(new Query(query))).mapTo[Seq[Status]]

    futureStatuses.map(_.flatMap(getText).filter(_.length <= maxLength))
 }

}


/**
 * An actor that constructs replies to a given status.
 */
class BigramReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.SimpleTokenizer

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future
  implicit val timeout = Timeout(10 seconds)

  /**
   * Produce a reply to a status using bigrams
   */
  lazy val stopwords = tshrdlu.util.English.stopwords_bot
  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to reply stream")

    val text = stripLeadMention(status.getText).toLowerCase
    
    // Get a sequence of futures of status sequences (one seq for each query)

    val bigram = Tokenize(text)
      .sliding(2)
      .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
      .flatMap{case Vector(x,y) => List(x+" "+y)}
      .toList
      .sortBy(-_.length)

    val statusSeqFutures: Seq[Future[Seq[String]]] = bigram
      .takeRight(5)
      .map(w => (context.parent ? SearchTwitter(new Query("\""+w+"\""))).mapTo[Seq[Status]].map(_.flatMap(getText).toSeq))
    
    //statusSeqFutures.foreach(println)
    // Convert this to a Future of a single sequence of candidate replies
    val statusesFuture: Future[Seq[String]] =
      extractText(statusSeqFutures,bigram.toList)

    //statusesFuture.foreach(println)
    // Filter statuses to their text and make sure they are short enough to use.
    statusesFuture.filter(_.length <= maxLength)
  }

  def extractText(statusList: Seq[Future[Seq[String]]],bigram:List[String]): Future[Seq[String]] = {
    val bigramMap = Future.sequence(statusList).map(_.flatten)
    //bigramMap.foreach(println)
    val sortedMap = bigramMap.map { tweet => {
      tweet.flatMap{ x => { 
        Tokenize(x)
          .sliding(2)
          .filterNot(z => (stopwords.contains(z(0))||stopwords.contains(z(1))))
          .map(bg => bg.mkString(" ") -> x) toMap
      }}.filter { case (p,q) => bigram.contains(p)}
    }}

    val bigramSeq = sortedMap.map(_.map(_._2))
    bigramSeq
  }

  def getText(status: Status): Option[String] = {
    import tshrdlu.util.English.{isEnglish,isSafe}

    val text = status.getText match {
      case StripMentionsRE(rest) => rest
      case x => x
    }
    
    if (!text.contains('@') && !text.contains('/') && isEnglish(text) && isSafe(text))
      Some(text)
    else None
  }

  
  def Tokenize(text: String): IndexedSeq[String]={
    val starts = """(?:[#@])|\b(?:http)"""
    text
    .replaceAll("""([\?!()\";\|\[\].,':])""", " $1 ")
    .trim
    .split("\\s+")
    .toIndexedSeq
    .filterNot(x => x.startsWith(starts))
  }

}

/**
 * An actor that constructs replies to a given status.
 */
class LuceneReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}

  import context.dispatcher
  import akka.pattern.ask
  import akka.util._
  import scala.concurrent.duration._
  import scala.concurrent.Future

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Trying to do search replies by Lucene")
    val text = status.getText.toLowerCase
	  val StripLeadMentionRE(withoutMention) = text
	  val query = SimpleTokenizer(withoutMention)
	    .filter(_.length > 2)
	    .toList
	    .mkString(" ")
      val replyLucene = Lucene.read(query)
    Future(replyLucene).map(_.filter(_.length <= maxLength))
  }

}

/**
 * A replier that replies based on unsupervised noun phrase chunking of a given tweet.
 */
class ChunkReplier extends BaseReplier {
  import Bot._
  import tshrdlu.util.{English, Lucene, SimpleTokenizer}
  import jarvis.nlp.TrigramModel
  import jarvis.nlp.util._
  import scala.concurrent.Future  
  import TwitterRegex._
  import akka.pattern.ask
  import akka.util._
  import context.dispatcher
  import scala.concurrent.duration._
  import scala.concurrent.Future
  import java.net.URL

  implicit val timeout = Timeout(10 seconds)

  //A Trigram language model based on a dataset of mostly english tweets
  val LanguageModel = TrigramModel(SPLReader(this.getClass().getResource("/chunking/").getPath()))

  val Chunker = new Chunker()

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Getting chunk tweets")
    val text = status.getText.toLowerCase
    val StripLeadMentionRE(withoutMention) = text
    val selectedChunks = Chunker(withoutMention)
      .map(c => (LanguageModel(SimpleTokenizer(c)), c))
      .sorted
      .take(2)
      .map(_._2)
    
     val statusList: Seq[Future[Seq[Status]]] = selectedChunks
         .map(chunk => (context.parent ? SearchTwitter(new Query(chunk))).mapTo[Seq[Status]])

    val statusesFuture: Future[Seq[Status]] = Future.sequence(statusList).map(_.flatten)

    statusesFuture
      .map(status => extractText(status))
      .map(_.filter(_.length <= maxLength))
  }

  /**
   * Go through the list of Statuses, filter out the non-English ones,
   * strip mentions from the front, filter any that have remaining
   * mentions, and then return the head of the set, if it exists.
   */
   def extractText(statusList: Seq[Status]): Seq[String] = {
     val useableTweets = statusList
       .map(_.getText)
       .map {
         case StripMentionsRE(rest) => rest
         case x => x
       }.filter(tweet => tshrdlu.util.English.isEnglish(tweet) 
                       &&  tshrdlu.util.English.isSafe(tweet)
                       && !tweet.contains('@')
                       && !tweet.contains('/'))
      .map(t => (LanguageModel(SimpleTokenizer(t)), t))
      .sorted
      .reverse
      .map{ case (k,t) => t}
      //given the set of potential tweets, return the tweet that has
      //the highest probability according to the language model
      Seq(if (useableTweets.isEmpty) "I don't know what to say." else useableTweets.head)
  }
}

/**
 * An actor that responds to requests to make sandwiches.
 *
 * @see <a href="http://xkcd.com/149/">http://xkcd.com/149/</a>
 */
class SudoReplier extends BaseReplier {
  import scala.concurrent.Future
  import context.dispatcher

  lazy val MakeSandwichRE = """(?i)(?:.*(\bsudo\b))?.*\bmake (?:me )?an?\b.*\bsandwich\b.*""".r

  def getReplies(status: Status, maxLength: Int = 140): Future[Seq[String]] = {
    log.info("Checking for sandwich requests")
    val text = TwitterRegex.stripLeadMention(status.getText)
    val replies: Seq[String] = Seq(text) collect {
      case MakeSandwichRE(sudo) => {
        Option(sudo) match {
          case Some(_) => "Okay."
          case None => "What? Make it yourself."
        }
      }
    }
    Future(replies.filter(_.length <= maxLength))
  }
}

/** An actor that responds to a tweet if it can be replied 
* by "Thats what she said". This is based on the work done by 
*Chloe Kiddon and Yuriy Brun , University of Washington
*That's What She Said: Double Entendre Identification
* Dataset from Edwin Chen
*/

class TWSSReplier extends BaseReplier {
  import scala.concurrent.Future
  import context.dispatcher
  import de.bwaldvogel.liblinear._; 
  import scala.collection.mutable.ArrayBuffer
  import tshrdlu.util.{TWSSModel, English,SimpleTokenizer}


  val vocabulary = English.vocabularyTWSS.map(line => line.split(" ")(0)).toIndexedSeq
  val IDFMap:Map[String,Int] = English.vocabularyTWSS.map { line=>
    val tokens = line.split(" ");
    (tokens(0),tokens(1).toInt)
  }.toMap

  def getReplies(status: Status , maxLength:Int = 140): Future[Seq[String]] ={
    log.info("Checking if tweet can be responded with TWSS")
    val tweet = TwitterRegex.stripLeadMention(status.getText.toLowerCase)
    val tweetMap:Map[String,Int] = SimpleTokenizer(tweet)
    .groupBy(x=> x)
    .mapValues(x=> x.length)

    val twssModel = TWSSModel()
    val featureVector = getFeatureVector(tweetMap)
    val prob = Array(0.0,0.0);
    Linear.predictProbability(twssModel, getFeatureVector(tweetMap).toArray,prob);
   // println(prob.toList);
    val response = if(prob.toList(0) > 0.9 ) "Thats what she said !! :-P " else "Thats was exactly what I told him !! "
    Future(Seq(response));
  }
  def getFeatureVector(document:Map[String,Int]): ArrayBuffer[Feature] ={
    val feature = new ArrayBuffer[Feature](vocabulary.size);
    var index=1;

    vocabulary.foreach{ word=>
      
      if(document.contains(word))
      {
      val tf = document(word);
      val idf = Math.log(7887/IDFMap(word));
      val tf_idf = tf*idf;
      val featureNode:Feature = new FeatureNode(index,tf_idf);
      feature += featureNode
      }
      index +=1
    }
    feature
  }
}

class SentimentReplier extends BaseReplier {
  import Bot._
  import TwitterRegex._
  import tshrdlu.util.{Polarity, SimpleTokenizer}

  import context.dispatcher
  import scala.concurrent.duration._
  import scala.concurrent.Future
  import akka.pattern._
  import akka.util._

  lazy val polarity = new Polarity()
  implicit val timeout = Timeout(10 seconds)

  def getReplies(status: Status, maxLength: Int): Future[Seq[String]] = {
    log.info("Trying to reply with SentimentReplier")

    val StripLeadMentionRE(withoutMention) = status.getText.toLowerCase
    val statusList =
      SimpleTokenizer(withoutMention)
      .filter(_.length > 3)
      .filter(_.length < 10)
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isSafe)
      .sortBy(- _.length)
      .toList
      // Use a bigram instead of a unigram search
      .take(4)
      .sliding(2)
      .map(_.mkString(" "))
      .map(w => (context.parent ? SearchTwitter(new Query(w))).mapTo[Seq[Status]])

    val statusesFuture: Future[Seq[Status]] = Future.sequence(statusList).map(_.toSeq.flatten)

    statusesFuture
      .map(status => extractText(status, withoutMention))
      .map(_.filter(_.length <= maxLength))
  }

  def extractText(statusList: Seq[Status], tweet: String): Seq[String] = {
    val desiredSentiment = getSentiment(tweet)
    val mult = if(desiredSentiment == 0) -1 else 1
    val useableTweets = statusList
      .map(_.getText)
      .map {
	case StripMentionsRE(rest) => rest
	case x => x
      }
      .filterNot(_.contains('@'))
      .filterNot(_.contains('/'))
      .filter(tshrdlu.util.English.isEnglish)
      .filter(tshrdlu.util.English.isSafe)

    val response = if (useableTweets.isEmpty) "Sorry, but I can't help you with that." else useableTweets.sortBy(x => mult * Math.abs(getSentiment(x) + mult * desiredSentiment)).head
    log.info("SentimentReplier responding with: " + response)
    Seq(response)
  }

  def getSentiment(text: String) = {
    val words = SimpleTokenizer(text)
    val len = words.length.toDouble
    val percentPositive = words.count(polarity.posWords.contains) / len
    val percentNegative = words.count(polarity.negWords.contains) / len
    (percentPositive - percentNegative)
  }
}
