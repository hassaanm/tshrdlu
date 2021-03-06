package gpp.exp

import chalk.lang.eng.PorterStemmer
import chalk.lang.eng.Twokenize
import nak.data._
import nak.util.ConfusionMatrix
import nak.liblinear.LiblinearConfig
import nak.NakContext._
import scala.xml.Elem
import scala.collection.mutable

/** The business classifier used to classify snippets of New York Times articles and their expected investment return. */
object Business {

    import tshrdlu.util.{English, Polarity, AlphaNumericTokenizer, SimpleTokenizer, CompanyData}
    lazy val stemmer = new PorterStemmer
    lazy val polarity = new Polarity()

    /** Runs the business classifier
      *
      * @param train a list of XML elements containing the training data
      * @param eval a list of XML elements containing the testing data
      * @param costValue the cost value of the classifier
      * @param detailed boolean to display verbose output
      * @param classifierFile string of the file name the classifier should save to
               classifier only saves to file if the length of classifierFile is greater than 0
      */
    def apply(train: List[Elem], eval: List[Elem], costValue: Double, detailed: Boolean, classifierFile: String) {
        val trainLabels = (for(file <- train) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val trainText = (for(file <- train) yield
            (file \\ "content").map(_.text).toList
        ).flatten
        
        val evalLabels = (for(file <- eval) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val evalText = (for(file <- eval) yield
            (file \\ "content").map(_.text).toList
        ).flatten

        lazy val featurizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val originalTokens = Twokenize(input)
                val tokens = originalTokens.map(_.toLowerCase).map(stemmer(_))
                    .filterNot(x => {
                         English.stopwords.contains(x) &&
                        !English.negationWords.contains(x)
                    })
                val wordCounts = tokens.groupBy(x=>x).mapValues(_.length).toList
                val basicFeatures = for ((word, count) <- wordCounts)
                    yield FeatureObservation(word+"="+count)
                val polarity = List(FeatureObservation("polarity="+sentimentLabel(getSentiment(tokens))))
                //subjectivity doesn't work :/
                val subjectivity = List(FeatureObservation("subjectivity="+getSubjectivity(tokens)))
                val dollarSign = List(FeatureObservation("dollar="+input.count(_ == '$')))
                val companies = extractCompanySymbols(tokens)
                val companyFeatures = List(FeatureObservation("companies="+companies.mkString(",")))
                (
                    basicFeatures ++ 
                    polarity ++ 
                    dollarSign ++ 
                    companyFeatures
                    )
            }
        }

        val trainExamples = for ((label, text) <- trainLabels.zip(trainText))
            yield Example(label, text)

        val config = LiblinearConfig(cost=costValue)
        val classifier = trainClassifier(config, featurizer, trainExamples)

        def maxLabelPpa = maxLabel(classifier.labels) _
        
        val predictions = for(text <- evalText) yield maxLabelPpa(classifier.evalRaw(text))

        val cm = ConfusionMatrix(evalLabels, predictions, evalText)
        println(cm)
        if(detailed)
            println(cm.detailedOutput)

        if(classifierFile.length > 0)
            saveClassifier(classifier, classifierFile)
    }

    /** Determines the sentiment label based on the sentiment value
      *
      * @param sentiment double value of the sentiment
      * @return string of the sentiment label
      */
    def sentimentLabel(sentiment: Double): String = {
        if (sentiment > 0.1)
            "positive"
        else if (sentiment < -0.1)
            "negative"
        else
            "neutral"
    }

    /** Extracts the possible company symbols based on a list of words
      *
      * @param words list of strings containing the words from article text
      * @return list of strings containing possible company mentions
      */
    def extractCompanySymbols(words: List[String]): List[String] = {
        val companyMentions = (for(word <- words) yield CompanyData.invertedIndex.get(word))
            .flatten
            .flatten
            .groupBy(identity)
            .mapValues(x => x.length)
            .toList
            .sortBy(-_._2)
        if(companyMentions.size == 0)
            return List[String]()
        val mostMentions = companyMentions.head._2
        val topMentionedCompanies = companyMentions.filter(_._2 == mostMentions).map(_._1)
        //println("Mentions: " + companyMentions.mkString(", "))
        //println("Most mentions: " + topMentionedCompanies.mkString(", "))
        //println(allCompanies.mkString(", "))
        //println("Confusing words: " + confusingWords.mkString(", "))
        topMentionedCompanies
    }

    /** Determines the sentiment polarity of provided text
      *
      * @param words list of strings containing the text to be analyzed
      * @return double containing the sentiment of the text
      */
    def getSentiment(words: List[String]): Double = {
        var numPos = words.take(1).count(polarity.posWords.contains)
        var numNeg = words.take(1).count(polarity.negWords.contains)
        for(wordSet <- words.sliding(2)){
            val negate = English.negationWords.contains(wordSet(0))
            if (polarity.posWords.contains(wordSet(1))){
                if(negate) {
                    numNeg += 1
                }
                else {
                    numPos += 1
                }
            }
            if (polarity.negWords.contains(wordSet(1))){
                if(negate) {
                    numPos += 1
                }
                else {
                    numNeg += 1
                }
            }
        }
        val sentiment = (
            if (numPos != 0 || numNeg != 0) { 
                (numPos - numNeg).toDouble / (numPos + numNeg) 
            }
            else 0)
        sentiment
    }

    /** Determines the subjectivity of provided text
      *
      * @param words list of strings containing the text to be analyzed
      * @return double containing the subjectivity of the text
      */
    def getSubjectivity(words: List[String]):Double = {
        val numPos = words.count(polarity.posWords.contains)
        val numNeg = words.count(polarity.negWords.contains)
        (numPos + numNeg) / words.size
    }
}
