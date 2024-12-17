import scala.io.Source

object RestroomRedoubt {

  case class Robot(x: Long, y: Long, dx: Long, dy: Long)

  val height = 103
  val width = 101

  def compute(lines: List[String], secs: Int): Long = {
    var (q1, q2, q3, q4) = (0L, 0L, 0L, 0L)
    simulate(lines.map(parse), secs).foreach { case (x, y) => 
      if(x < 50 && y < 51) q1 += 1
      else if (x > 50 && y < 51) q2 += 1
      else if (x < 50 && y > 51) q3 += 1
      else if (x > 50 && y > 51) q4 += 1
    }

    q1 * q2 * q3 * q4
  }

  def simulate(robots: List[Robot], secs: Int): List[(Long, Long)] = 
    robots.map { r =>
      val x = ((r.x + r.dx * secs) % width + width) % width
      val y = ((r.y + r.dy * secs) % height + height) % height
      (x, y)
    }

  def parse(line: String): Robot = 
    line match {
      case s"p=$x,$y v=$dx,$dy" => Robot(x.toLong, y.toLong, dx.toLong, dy.toLong)
    }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val solution = compute(input, secs = 100)
    println(s"Solution: $solution")

  }
}
