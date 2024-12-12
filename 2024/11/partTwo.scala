import scala.io.Source
import scala.collection.mutable.Map 

object PlutonianPebbles2 {

  type MemoMap = Map[(Int, Long), Long]

  def parseInput(input: String): List[Long] = 
    input.filter(_ != '\n').split(' ').map(_.toLong).toList

  def blink(stone: Long): Seq[Long] = stone match {
    case 0 => Seq(1)
    case n if stone.toString.length % 2 == 0 => 
      val parts = n.toString.splitAt(n.toString.length / 2)
      Seq(parts._1.toLong, parts._2.toLong)
    case n => Seq(n * 2024)
  }

  def recurse(depth: Int, stone: Long, memo: MemoMap): Long = {
    (depth, stone) match {
      case (75, _) => 1
      case x => 
        memo.getOrElseUpdate(x, blink(stone).map(recurse(depth + 1, _, memo)).sum)
    }
  }

  def solve(input: List[Long]): Long = {
    val memo = Map[(Int, Long), Long]()
    input.map(recurse(0, _, memo)).sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val solution = solve(parseInput(input))
    println(s"Solution: $solution")
  }
}
