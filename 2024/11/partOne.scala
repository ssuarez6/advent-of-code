import scala.io.Source

object PlutonianPebbles {

  def parseInput(input: String): List[Long] = 
    input.filter(_ != '\n').split(' ').map(_.toLong).toList

  def blink(stone: Long): Seq[Long] = stone match {
    case 0 => Seq(1)
    case n if stone.toString.length % 2 == 0 => 
      val parts = n.toString.splitAt(n.toString.length / 2)
      Seq(parts._1.toLong, parts._2.toLong)
    case n => Seq(n * 2024)
  }

  def solve(input: List[Long]): Long = 
    Iterator.iterate(input)(_.flatMap(blink)).drop(25).next.size

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val solution = solve(parseInput(input))
    println(s"Solution: $solution")
  }
}
