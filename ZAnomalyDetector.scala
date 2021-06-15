import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.internal.util.Collections

object ZAnomalyDetector extends AnomalyDetector {

  override def learn(normal: TimeSeries): Map[String, String] = { // This function return map and each pair is the feature and max zscore val
    (normal.features zip normal.featuresMap.values.map(arr => {
      (arr.map(item => Math.abs(Util.zscore(arr.toArray, item))).toList).max.toString
    }))
      .toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var errors: ListBuffer[(String, Int)] = ListBuffer()
    var maxVal: Double = 0
    test.featuresMap foreach (pair => { // Iterating the map
      val feature = pair._1 // Getting the key
      maxVal = model.get(feature).get.toDouble // Getting the max value for each feature
      val dataAsArray = pair._2.toArray
      for (i <- 0 until dataAsArray.length ) {
        val zScore = Math.abs(Util.zscore(dataAsArray, dataAsArray(i)))
        if (zScore > maxVal) {
          val error = (feature, i)
          errors += error
        }
      }
    })


    errors.toVector
  }

}
