import scala.io.Source

object RestroomRedoubt2 {

  case class Robot(x: Long, y: Long, dx: Long, dy: Long)

  case class Coord(i: Long, j: Long) {
    def neighbour4: List[Coord] = List(
        (i + 1, j),
        (i - 1, j),
        (i, j -1),
        (i, j + 1)
      ).map(p => Coord(p._1, p._2))
  }

  val height = 103
  val width = 101

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

  def getClusters(list: List[Coord]): List[Set[Coord]] = {
    list.foldLeft(List.empty[Set[Coord]]) { (sets, c) => 
      val (cluster, others) = sets.partition(set => c.neighbour4.exists(set))
      (Set(c) :: cluster).reduce(_ ++ _) :: others
    } 
  }

  def scheme(robots: List[Robot], secs: Int): String = {
    val arr = Array.fill(height)(Array.fill(width)(' '))
    simulate(robots, secs).foreach { case (x, y) =>
      arr(y.toInt)(x.toInt) = '*'
    }
    arr.map(_.mkString("")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val robots = input.map(parse)
    (0 to Int.MaxValue).foreach {secs =>
      val positions = simulate(robots, secs)
      val clusters = getClusters(positions.map(p => Coord(p._1, p._2)))
      if(clusters.exists(_.size >= 20)) {
        println(secs)
        println(scheme(robots, secs))
      }
    }

  }
}
