import scala.io.Source

object Solution {
  def main(args: Array[String]) = {
    val input = Source.stdin.getLines()

    val (left, right) = input.map { line => 
      val parts = line.trim.split("\\s+").map(_.toInt)
      (parts(0), parts(1))
    }.toList.unzip


    val solution = sumPairs(left, right)
    println(s"Solution: $solution")
  }

  def sumPairs(xs: List[Int], ys: List[Int]): Int = {
    (xs.sorted zip ys.sorted).foldLeft(0) {
      case (acc, (x, y)) => acc + (x-y).abs
    }
  }
}
