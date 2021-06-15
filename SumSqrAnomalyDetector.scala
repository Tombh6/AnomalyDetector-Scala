
import LinearRegAnomalyDetector.{getLine, linearRegLine}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object SumSqrAnomalyDetector extends AnomalyDetector {
  override def learn(normal: TimeSeries): Map[String, String] = {
    var map: mutable.HashMap[String, String] = mutable.HashMap()
    val couples = Util.getCorrelativeCouples(normal)

    couples.foreach(pair => {
      val points = Util.getFeaturesAsPoints(pair._1, pair._2, normal)
      val pointsWithIndex = points.zipWithIndex
      val temp = pointsWithIndex.map(point => Util.findDistSum(point._1, points))
      val max = (temp.max)
      map.put(s"${pair._1}" + "," + s"${pair._2}", max.toString)
    })
    map.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {

    var errors: ArrayBuffer[(String, Int)] = ArrayBuffer()
    model foreach (pair => {
      val feature1 = pair._1.split(",")(0)
      val feature2 = pair._1.split(",")(1)
      val maxSum = pair._2.toDouble
      val points = Util.getFeaturesAsPoints(feature1, feature2, test)

      val pointsWithIndex = points.zipWithIndex
      val temp = pointsWithIndex.map(point => {
        val  x = Util.findDistSum(point._1, points)
        if(x > maxSum) {
          val error = (feature1+ "," + feature2,point._2)
          errors += error
        }
      })

    })
    errors.toVector

  }


}