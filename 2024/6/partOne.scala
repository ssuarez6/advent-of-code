import scala.io.Source

object GuardGallivant {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val initialGuard = findStartingPosition(input)
    val finalPosition = advanceUntilOutOfBounds(initialGuard, input)
    println(s"Solution: ${finalPosition.visited.size}")
  }

  def findStartingPosition(grid: List[String]): Guard = {
    val directions = Map('>' -> '>', '<' -> '<', '^' -> '^', 'v' -> 'v')
    val position = for {
      row <- grid.indices
      col <- grid(row).indices
      if directions.contains(grid(row)(col))
    } yield ((row, col), directions(grid(row)(col)))
    val ((row, col), dir) = position.head
    Guard((row, col), dir, Set((row, col)))
  }

  def advanceInDirection(pos: (Int, Int), dir: Char): (Int, Int) = {
    dir match {
      case '>' => (pos._1, pos._2+1)  // Right
      case '<' =>  (pos._1, pos._2-1) // Left
      case '^' =>  (pos._1-1, pos._2) // Up
      case 'v' =>  (pos._1+1, pos._2) // Down
      case _ => throw new IllegalStateException("Bad direction")
    }
  }

  def isOutOfBounds(pos: (Int, Int), grid: List[String]): Boolean = 
    pos._1 > grid.size-1 || pos._2 > grid.head.size-1 || pos._1 < 0 || pos._2 < 0

  def isObstacle(pos: (Int, Int), grid: List[String]): Boolean = 
    !isOutOfBounds(pos, grid) && grid(pos._1)(pos._2) == '#' 

  def rotate90Degrees(dir: Char): Char = {
    dir match {
      case '>' => 'v'
      case 'v' =>  '<'
      case '<' =>  '^'
      case '^' =>  '>'
      case _ => throw new IllegalStateException("Bad direction")
    }
  }

  def advanceUntilOutOfBounds(guard: Guard, grid: List[String]): Guard = {
    def helper(guard: Guard): Guard = {
      val nextCell = advanceInDirection(guard.position, guard.direction)
      if (isOutOfBounds(nextCell, grid)) guard
      else if (isObstacle(nextCell, grid)) {
        val rotatedGuard = guard.copy(direction = rotate90Degrees(guard.direction))
        helper(rotatedGuard)
      } else {
        val advancedGuard = guard.copy(position = nextCell, visited = guard.visited + nextCell)
        helper(advancedGuard)
      }
    }
    helper(guard)
  }

  case class Guard(position: (Int, Int), direction: Char, visited: Set[(Int, Int)])
}
