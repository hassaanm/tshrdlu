package gpp.exp

import chalk.lang.eng.PorterStemmer
import chalk.lang.eng.Twokenize
import nak.data._
import nak.util.ConfusionMatrix
import nak.liblinear.LiblinearConfig
import nak.NakContext._
import scala.xml.Elem

/** The supervised classifier based on L2-regularized logistic regression. */
object Supervised {

    import tshrdlu.util.{English, Polarity}
    lazy val polarity = new Polarity()
    lazy val stemmer = new PorterStemmer()

    /** Runs the supervised classifier
      *
      * @param train a list of XML elements containing the training data
      * @param eval a list of XML elements containing the testing data
      * @param costValue the cost value of the classifier
      * @param extended boolean to determine which featurizer to use (basic or extended)
      * @param detailed boolean to display verbose output
      * @param classifierFile string of the file name the classifier should save to
               classifier only saves to file if the length of classifierFile is greater than 0
      */
    def apply(train: List[Elem], eval: List[Elem], costValue: Double, extended: Boolean, detailed: Boolean, classifierFile: String) {
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

        lazy val basicFeaturizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val tokens = Twokenize(input)
                val features = tokens.groupBy(x=>x).mapValues(_.length).toList
                for ((word, count) <- features)
                    yield FeatureObservation(word+"="+count)
            }
        }

        lazy val extendedFeaturizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val originalTokens = Twokenize(input)
                val tokens = originalTokens.map(_.toLowerCase).map(stemmer(_))
                val wordCounts = tokens.groupBy(x=>x).mapValues(_.length).toList
                val basicFeatures = for ((word, count) <- wordCounts)
                    yield FeatureObservation(word+"="+count)
                val polarity = List(FeatureObservation("polarity="+sentimentLabel(getSentiment(Twokenize(input)))))
                (basicFeatures ++ polarity)
            }
        }

        val featurizer = if (extended) extendedFeaturizer else basicFeaturizer

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

    /** Determines the sentiment polarity of provided text
      *
      * @param words list of strings containing the text to be analyzed
      * @return double containing the sentiment of the text
      */
    def getSentiment(words: List[String]): Double = {
        var numPos = words.take(1).count(polarity.posWords.contains)
        var numNeg = words.take(1).count(polarity.negWords.contains)
        for(wordSet <- words.sliding(2)){
            val word = if (wordSet.size > 1) wordSet(1) else wordSet(0)
            val negate = English.negationWords.contains(wordSet(0))
            if (polarity.posWords.contains(word)){
                if(negate) {
                    numNeg += 1
                }
                else {
                    numPos += 1
                }
            }
            if (polarity.negWords.contains(word)){
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
