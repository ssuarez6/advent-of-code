import scala.io.Source

object HoofIt2 {

  type Position = (Int, Int)
  implicit class PositionSyntax(pos: Position) {
    def +(other: Position): Position = (pos._1 + other._1, pos._2 + other._2)
  }

  type Topo = Vector[Vector[Int]]
  implicit class TopoSyntax(topo: Topo) {
    def apply(pos: Position): Int = topo(pos._1)(pos._2)

    def inBounds(pos: Position): Boolean = 
      pos._1 >= 0 && pos._1 < topo.size &&
      pos._2 >= 0 && pos._2 < topo.head.size

    def positions = 
      for {
        row <- topo.indices
        column <- topo.head.indices
      } yield (row, column)

  }

  type Graph = Map[Position, Set[Position]]

  def computeGraph(topo: Topo): Graph = {
    def reachableNeighbors(pos: Position): Set[Position] = {
      Set((-1, 0), (1, 0), (0, -1), (0, 1))
        .flatMap(offsets => Some(pos + offsets))
        .filter(nextPos => topo.inBounds(nextPos) && topo(nextPos) == topo(pos) + 1)
    }
    topo.positions
      .map(pos => pos -> reachableNeighbors(pos))
      .toMap
  }

  def solve(topo: Topo): Int = {
    val graph = computeGraph(topo)
    def routes(pos: Position): Int = 
      if (topo(pos) == 9) 1
      else graph(pos).toSeq.map(routes).sum
    topo.positions
      .filter(pos => topo(pos) == 0)
      .map(routes)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val input: Topo = Source.fromFile("input.txt").getLines.map(_.map(_.asDigit).toVector).toVector
    val solution: Int = solve(input)
    println(s"Solution: $solution")
  }
}
