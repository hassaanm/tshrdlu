package gpp.exp

import nak.util.ConfusionMatrix
import scala.xml.Elem

/** The majority classifier, which uses the majority label for everything. */
object Majority {

    /** Runs the majority classifier
      *
      * @param train a list of XML elements containing the training data
      * @param eval a list of XML elements containing the testing data
      * @param detailed boolean to display verbose output
      */
    def apply(train: List[Elem], eval: List[Elem], detailed: Boolean) {
        val trainLabels = (for(file <- train) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        
        val majorityLabel = trainLabels.groupBy(x=>x).mapValues(_.length).toList.sortBy(-_._2).head._1
        
        val evalLabels = (for(file <- eval) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val evalText = (for(file <- eval) yield
            (file \\ "content").map(_.text).toList
        ).flatten
        
        val predictions = List.fill(evalLabels.length)(majorityLabel)
        val cm = ConfusionMatrix(evalLabels, predictions, evalText)
        println(cm)
        if(detailed)
            println(cm.detailedOutput)
    }
}
