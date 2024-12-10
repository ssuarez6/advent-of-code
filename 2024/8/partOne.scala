import scala.collection.mutable
import scala.io.Source

object ResonantCollinearity {
  type Position = (Int, Int)
  type PositionPair = (Position, Position)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val antennas = getAntenas(input)
    val allAntinodes = antennas.foldLeft(Set.empty[Position]) { case (acc, (freq, pos)) =>
      val pairs = generateUniquePairs(pos)
      val antinodesForPairs = pairs.foldLeft(Set.empty[Position]){ (acc1, pair) => 
        acc1 ++ placeAntinodesFor(pair, input)
      }
      acc ++ antinodesForPairs.filterNot(antennas.values.toList.contains)
    }
    val solution = allAntinodes.size
    println(s"Solution: $solution")
  }

  def getAntenas(map: List[String]): Map[String, List[Position]] = {
    val mappedAntennas = mutable.Map[String, List[Position]]().withDefaultValue(List.empty)
    for {
      row <- map.indices
      col <- map(row).indices
      if(map(row)(col) != '.') 
    } {
      val freq = map(row)(col).toString
      val position = (row, col)
      mappedAntennas(freq) = position :: mappedAntennas(freq)
    }
    mappedAntennas.toMap
  }

  def generateUniquePairs(ps: List[Position]): List[PositionPair] = {
    for {
      (p1, i) <- ps.zipWithIndex
      p2 <- ps.drop(i+1)
    } yield (p1, p2)
  }

  def placeAntinodesFor(antennas: PositionPair, grid: List[String]): Set[Position] = {
    val ((x1, y1), (x2, y2)) = antennas
    val maxRows = grid.length
    val maxCols = grid.head.length

    def isInBound(pos: Position): Boolean = {
      val (x, y) = pos
      x >= 0 && x < maxRows && y >= 0 && y < maxCols
    }
    
    val dx = x2 - x1
    val dy = y2 - y1
    val antinode1 = (x1 - dx, y1 - dy)
    val antinode2 = (x2 + dx, y2 + dy)

    Set(antinode1, antinode2).filter(isInBound)
  }
}
