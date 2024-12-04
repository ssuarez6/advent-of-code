import scala.io.Source

object MullItOver2 {
  def main(args: Array[String]): Unit = {

    val input = Source.fromFile("input.txt").getLines.reduceLeft(_ + _)

    println(s"Solution: ${computeInstructions(input)}")

  }

  def computeInstructions(s: String): Long = {
    val mulRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val doRegex = """do\(\)""".r
    val dontRegex = """don't\(\)""".r
    val tokens = s"""($mulRegex|$doRegex|$dontRegex)""".r.findAllIn(s).toList

    tokens.foldLeft((0L, true)) { case ((acc, enabled), token) => 
      token match {
        case mulRegex(first, second) if enabled => (acc + (first.toLong * second.toLong), enabled)
        case mulRegex(_, _) => (acc, enabled)
        case doRegex() => (acc, true)
        case dontRegex() => (acc, false)
        case _ => (acc, enabled)
      }
    }._1
  }
}
