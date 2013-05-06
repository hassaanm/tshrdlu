package gpp.exp

import chalk.lang.eng.Twokenize
import nak.util.ConfusionMatrix
import scala.xml.Elem

/** The lexicon classifier, which uses the Bing Liu's sentiment analyzer for classification. */
object Lexicon {

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
        
        val predictions = (for(text <- evalText) yield getSentiment(text))
        val cm = ConfusionMatrix(evalLabels, predictions, evalText)
        println(cm)
        if(detailed)
            println(cm.detailedOutput)
    }
    
    /** Determines the sentiment polarity of provided text
      *
      * @param text string containing the text, which the method determines the sentiment of
      * @return string of the sentiment polarity of the text
      */
    def getSentiment(text: String): String = {
        val tokens = Twokenize(text)
        val polarity = English.getPolarity(tokens)
        return polarity match {
            case 0 => "positive"
            case 1 => "negative"
            case 2 => "neutral"
        }
    }
}
