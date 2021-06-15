
import java.util.concurrent.{Callable, ExecutorService, Future}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]

  def map(ts: TimeSeries): Reports

  def reduce(r1: Reports, r2: Reports): Reports

  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val chunkSize = ts.calcChunkSize(chunks)
    ts.split(chunks)
      .map(chunk => es.submit(new Callable[Reports] {
        override def call(): Reports = {
          map(chunk)
        }
      }))
      .map(future => future.get())
      .zipWithIndex
      .map(reportsWithIndex => {
        val repos = reportsWithIndex._1
        val inx = reportsWithIndex._2
        repos.foreach(report => {
          report.timeStep = (inx * chunkSize) + report.timeStep
        })

        repos
      })
      .reduce((x, y) => reduce(x, y))
      .toVector
  }
}


