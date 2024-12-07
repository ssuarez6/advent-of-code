import scala.io.Source

object BridgeRepair {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val equations = input.map(parseEquation)
    val solution = equations.filter(eq => isSolvable(eq._1, eq._2)).map(_._1).sum
    println(s"Solution: $solution")
  }

  def parseEquation(line: String): (Long, List[Long]) = {
    val Array(resultStr, numbersStr) = line.split(":")
    val numbers = numbersStr.trim.split(" ").map(_.toLong).toList
    (resultStr.toLong, numbers)
  }

  def applyOperators(factors: List[Long], ops: List[Char]): Long = {
    factors.tail.zip(ops).foldLeft(factors.head) {
      case (result, (num, op)) => 
        op match {
          case '+' => result + num
          case '*' => result * num
          case _ => throw new IllegalArgumentException("Operator can only be '+' or '*'")
        }
    }
  }

  def genNOperators(n: Int): List[List[Char]] = {
    n match {
      case 0 => List(List())
      case _ => {
        val smaller = genNOperators(n-1)
        smaller.flatMap(comb => List('+' :: comb, '*' :: comb))
      }
    }
  }

  def isSolvable(result: Long, factors: List[Long]): Boolean = {
    val opsCombinations = genNOperators(factors.size - 1)
    opsCombinations.exists(ops => applyOperators(factors, ops) == result)
  }
}
