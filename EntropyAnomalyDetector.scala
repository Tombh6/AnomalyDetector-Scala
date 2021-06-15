
import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector {


  override def map(tims: TimeSeries): Reports = {
    var reports = new Reports()
    tims.features.foreach(feature => {
      val ves = tims.getValues(feature).get.toArray
      val maxEntForFeature = ves
        .zipWithIndex
        .map(xWithIndex => {
          val x = xWithIndex._1
          val index = xWithIndex._2

          (Util.entropyDiff(ves, index), index)
        })
        .maxBy(pair => pair._1)

      reports += Report(feature, maxEntForFeature._2, maxEntForFeature._1)
    })
    reports
  }

  override def reduce(r1: Reports, r2: Reports): Reports = {
    (r1 zip r2).map(r => {
      if (r._1.anomalyScore > r._2.anomalyScore) {
        r._1
      } else {
        r._2
      }
    })
  }
}

