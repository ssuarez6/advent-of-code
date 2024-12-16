import scala.io.Source

object ClawContraption2 {

  implicit class LongSyntax(l: Long) {
    def safeDiv(b: Long): Option[Long] = 
      Option.when(b != 0 && l % b == 0)(l / b)
  }

  object L {
    def unapply(s: String): Option[Long] = s.toLongOption
  }

  case class Claw(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long) {
    def solve: Option[Long] = for {
      b <- (x * ay - y * ax).safeDiv(bx * ay - by * ax)
      a <- (x - b * bx).safeDiv(ax)
    } yield a * 3 + b
  }

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

  
  def main(args: Array[String]): Unit = {
    val diff = 10_000_000_000_000L
    val input = Source.fromFile("input.txt").mkString
    val claws: Seq[Claw] = parse(input).map(c => c.copy(x = c.x+diff, y = c.y + diff))
    val solution: Long = claws.flatMap(_.solve).sum
    println(s"Solution: $solution")
  }
}
