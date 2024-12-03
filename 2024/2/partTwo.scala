import scala.io.Source

object RedNosedReports2 {
  
  def main(args: Array[String]): Unit = {

    val input = Source.stdin.getLines()

    val reports: Array[Array[Int]] = input.map(line => line.split(" ").map(_.toInt)).toArray

    val solution: Int = reports.filter(r => isSafe(r) || couldBeSafe(r)).size

    println(s"Solution: $solution")
  }

  def isSafe(xs: Array[Int]): Boolean = {
    val changeFactors = xs.sliding(2).map {
      case Array(x1, x2) => x2-x1
    }.toArray
    (changeFactors.forall(_ > 0) || changeFactors.forall(_ < 0)) && (changeFactors.forall(f => f.abs <= 3))
  }

  def couldBeSafe(xs: Array[Int]): Boolean = {
    val allReports = xs.indices.map {i => 
      xs.take(i) ++ xs.drop(i+1)
    }

    allReports.exists(isSafe)
  }
}
