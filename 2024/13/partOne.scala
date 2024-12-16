import scala.io.Source

object ClawContraption {


  object L {
    def unapply(s: String): Option[Long] = s.toLongOption
  }

  case class Claw(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long)

  object Claw {
    def parse(xs: Seq[String]): Option[Claw] = xs match {
      case Seq(
          s"Button A: X+${L(ax)}, Y+${L(ay)}",
          s"Button B: X+${L(bx)}, Y+${L(by)}",
          s"Prize: X=${L(x)}, Y=${L(y)}"
        ) => 
        Some(Claw(ax, ay, bx, by, x, y))
      case _ => None
    }
  }

  def parse(s: String): Seq[Claw] =
    s.split("\n+").toSeq.grouped(3).flatMap(Claw.parse).toSeq

  def solve(c: Claw): Seq[Long] = 
    for {
      a <- 0 to 100
      b <- 0 to 100
      if a * c.ax + b * c.bx == c.x
      if a * c.ay + b * c.by == c.y
    } yield (a * 3L + b)
  
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    val claws: Seq[Claw] = parse(input)
    val solution: Long = claws.flatMap(solve).sum
    println(s"Solution: $solution")
  }
}
