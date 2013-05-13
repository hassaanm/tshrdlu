package gpp.exp

import chalk.lang.eng.Twokenize
import nak.util.ConfusionMatrix
import scala.xml.Elem

/** The lexicon classifier, which uses the Bing Liu's sentiment analyzer for classification. */
object Lexicon {

    import tshrdlu.util.{English, Polarity}
    lazy val polarity = new Polarity()

    /** Runs the lexicon classifier
      *
      * @param eval a list of XML elements containing the testing data
      * @param detailed boolean to display verbose output
      */
    def apply(eval: List[Elem], detailed: Boolean) {
        val evalLabels = (for(file <- eval) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val evalText = (for(file <- eval) yield
            (file \\ "content").map(_.text).toList
        ).flatten
        
        val predictions = for(text <- evalText) yield sentimentLabel(getSentiment(Twokenize(text)))
        val cm = ConfusionMatrix(evalLabels, predictions, evalText)
        println(cm)
        if(detailed)
            println(cm.detailedOutput)
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
}
