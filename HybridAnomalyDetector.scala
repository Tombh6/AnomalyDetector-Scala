
import LinearRegAnomalyDetector.{getLine, linearRegLine}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object HybridAnomalyDetector extends AnomalyDetector {

  class linearRegLine(a: Double, b: Double, maxDist: Double) {
    val _a = a
    val _b = b
    val _maxDist = maxDist

    def f(x: Double): Double = _a * x + _b

    def dist(p: Point): Double = Math.abs(f(p.x) - p.y)

  }

  override def learn(normal: TimeSeries): Map[String, String] = {
    var map: mutable.HashMap[String, String] = mutable.HashMap()
    val x = Util.getCorrelativeCouples2(normal)

    x.foreach(item => {
      val pearsonVal = item._3

      if (pearsonVal >= 0.9) {
        val line = getLine(item._1, item._2, normal)
        map.put(s"${item._1}" + "," + s"${item._2}", line.toString)
      }

      else if (pearsonVal > 0.5 && pearsonVal < 0.9) {

        val points = Util.getFeaturesAsPoints(item._1, item._2, normal)
        val sumSqrVals = points.map(point => (Util.findDistSum(point, points)))
        val minSum = sumSqrVals.min
        var minPoint = new Point(0, 0)
        points.foreach(point => {
          if (Util.findDistSum(point, points) == minSum)
            minPoint = point
        })
        var maxPoint = new Point(0, 0)
        var maxDist = 0.0;
        points.foreach(point => {
          val dist = Util.calculateDistance(minPoint, point)
          if (dist > maxDist) {
            maxDist = dist
            maxPoint = point
          }
        })
        val raduis = Util.calculateDistance(minPoint, maxPoint)
        map.put(s"${item._1}" + "," + s"${item._2}", raduis.toString)
      }

      else { // pearson val <= 0.5
        val feature1 = item._1 // [1.0,2.1,3.2,...]
        val feature2 = item._2 // [1.0,2.1,3.2,...]
        val f1Vals = normal.featuresMap.get(feature1).get
        val f2Vals = normal.featuresMap.get(feature2).get
        val f1MaxVal = f1Vals.map(value => Math.abs(Util.zscore(f1Vals.toArray, value))).max
        val f2MaxVal = f2Vals.map(value => Math.abs(Util.zscore(f2Vals.toArray, value))).max
        map.put(feature1, f1MaxVal.toString)
        map.put(feature2, f2MaxVal.toString)
      }
    })


    map.toMap


  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var errors: ListBuffer[(String, Int)] = ListBuffer()

    model.foreach(pair => {

      val check = pair._1.split(",")

      if (check.length == 1) {


        val values = test.featuresMap.get(check(0)).get
        val maxzScoreVal = pair._2.toDouble
        values.zipWithIndex.foreach(item => {
          val calc = Util.zscore(values.toArray, item._1)
          if (Math.abs( calc) > maxzScoreVal) {
            val error = (check(0), item._2)
            errors += error
          }
        })

      }
      else if (check.length == 2) {

        val feature1 = pair._1.split(",")(0)
        val feature2 = pair._1.split(",")(1)
        val temp = pair._2;
        val args = temp.split(",")
        if (args.length == 3) { // Line

          val a = args(0).toDouble
          val b = args(1).toDouble
          val maxDist = args(2).toDouble
          val line = new linearRegLine(a, b, maxDist)
          val points = Util.getFeaturesAsPoints(feature1, feature2, test)
          for (i <- 0 until points.length) {
            val dist = line.dist(points(i))
            if (dist > line._maxDist) {
              val error = (s"${feature1}" + "," + s"${feature2}", i)
              errors += error
            }
          }

        }

        else if (args.length == 1) { // Circle
          val radius = pair._2.toDouble
          val points = Util.getFeaturesAsPoints(feature1, feature2, test)
          val sumSqrVals = points.map(point => (Util.findDistSum(point, points)))
          val minSum = sumSqrVals.min
          var minPoint = new Point(0, 0)
          points.foreach(point => {
            if (Util.findDistSum(point, points) == minSum)
              minPoint = point
          })
          points.zipWithIndex.foreach(point=>{
            val dist = Util.calculateDistance(minPoint,point._1)
            if(dist > radius) {
              val error = (feature1.toString + "," + feature2.toString, point._2)
              errors += error
            }
          })

        }
      }


    })

    errors.toVector
  }


  def getLine(f1: String, f2: String, timeSeries: TimeSeries): Line = {
    new Line(Util.getFeaturesAsPoints(f1, f2, timeSeries))
  }


}
