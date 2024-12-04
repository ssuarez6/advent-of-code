import scala.io.Source
object MullItOver {
  def main(args: Array[String]): Unit = {

    val input = Source.fromFile("input.txt").getLines.toList
    val solution = input.map(l => computeInstructions(l)).sum

    println(s"Solution: $solution")

  }

  def computeInstructions(s: String): Int = {
    val regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

    regex.findAllMatchIn(s).foldLeft(0){ (acc, m) => 
      val first = m.group(1).toInt
      val second = m.group(2).toInt
      acc + (first*second)
    }
  }
}
