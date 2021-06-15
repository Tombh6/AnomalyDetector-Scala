
import scala.io.Source

class TimeSeries(csvFileName: String) {

  private val source = Source.fromFile(csvFileName)
  val features = source.getLines.take(1).toVector(0).split(',').toVector
  source.close()

  val featuresMap =  {
   Source.fromFile(csvFileName).getLines()
      .map(_.split(","))
      .toVector
      .transpose.map(arr=>(arr.head,arr.tail.map(_.toDouble))).toMap
    //file is auto-closed
  }



  //  // given name of a feature return in O(1) its value series
  def getValues(feature: String): Option[Vector[Double]] = {
    if (!featuresMap.contains(feature))
      None
    featuresMap.get(feature)
  }

  //  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    if (!featuresMap.contains(feature))
      return None
    val dataAsVector = featuresMap.get(feature).get
    if (timeStep < 0 || timeStep >= dataAsVector.size)
      return None
    Option(dataAsVector(timeStep))
  }

  //  // given name of a feature return its value series in the range of indices
  def getValues(feature: String, r: Range): Option[Vector[Double]] = {
    if (!featuresMap.contains(feature))
      return None
    val data = featuresMap.get(feature)
    data match {
      case Some(data) => {
        val dataLength = data.size
        if (r.start >= dataLength || r.start < 0 || r.end >= dataLength || r.size > dataLength) // out of bounds
          return None
        else { // Bounds are ok
          return Option(data.dropRight((data.length - r.end - r.start)).drop(r.start)) // Trimming from the right and then left
        }
      }
      case None => None
    }
  }


}
