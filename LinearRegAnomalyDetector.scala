import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object LinearRegAnomalyDetector extends AnomalyDetector {

  class linearRegLine(a: Double, b: Double, maxDist: Double) {
    val _a = a
    val _b = b
    val _maxDist = maxDist
    def f(x: Double): Double = _a*x+_b
    def dist(p: Point): Double = Math.abs(f(p.x) - p.y)

  }

  override def learn(normal: TimeSeries): Map[String, String] = {
    var map: mutable.HashMap[String, String] = mutable.HashMap()
    val couples = Util.getCorrelativeCouples(normal)
    couples.foreach(pair => {
      val line = getLine(pair._1, pair._2, normal)
      map.put(s"${pair._1}" + "," + s"${pair._2}", line.toString)
    })
    map.toMap
  }

  override def detect(model: Map[String, String], test: TimeSeries): Vector[(String, Int)] = {
    var errors: ArrayBuffer[(String, Int)] = ArrayBuffer()
    val lines = model.keys zip model.values.map(str => str.split(","))

    lines foreach (pair => {
      val feature1 = pair._1.split(",")(0)
      val feature2 = pair._1.split(",")(1)
      val a = pair._2(0).toDouble
      val b = pair._2(1).toDouble
      val maxDist = pair._2(2).toDouble
      val line = new linearRegLine(a, b, maxDist)
      val points = Util.getFeaturesAsPoints(feature1, feature2, test)
      for (i <- 0 until points.length) {
        val dist = line.dist(points(i))
        if (dist > line._maxDist) {
          val error = (s"${feature1}" + "," + s"${feature2}", i)
          errors += error
        }
      }
    })
    errors.toVector
  }


  def getLine(f1: String, f2: String, timeSeries: TimeSeries): Line = {
    new Line(Util.getFeaturesAsPoints(f1, f2, timeSeries))
  }


}


