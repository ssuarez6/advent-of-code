import scala.io.Source

object GuardGallivant2 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList
    val initialGuard = findStartingPosition(input)
    val obstaculablePositions = findObstaculablePositions(initialGuard.position, input)
    val solution = obstaculablePositions.filter{pos =>
      val gridWithObstacle = placeObstacleAt(pos, input)
      getsStuckInLoop(initialGuard, gridWithObstacle)
    }
    println(s"Solution: ${solution.size}")
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

  def getsStuckInLoop(guard: Guard, grid: List[String]): Boolean = {
    def helper(guard: Guard, visitedStates: Set[((Int, Int), Char)]): Boolean = {
      val currentState = (guard.position, guard.direction)
      if (visitedStates.contains(currentState)) true
      else {
        val nextCell = advanceInDirection(guard.position, guard.direction)
        if (isOutOfBounds(nextCell, grid)) false
        else if (isObstacle(nextCell, grid)) {
          val rotatedGuard = guard.copy(direction = rotate90Degrees(guard.direction))
          helper(rotatedGuard, visitedStates + currentState)
        } else {
          val advancedGuard = guard.copy(position = nextCell, visited = guard.visited + nextCell)
          helper(advancedGuard, visitedStates + currentState)
        }
      }
    }
    helper(guard, Set.empty)
  }

  def findObstaculablePositions(initial: (Int, Int), grid: List[String]): List[(Int, Int)] = {
    (for {
      row <- grid.indices
      col <- grid(row).indices
      if (grid(row)(col) == '.' && (row, col) != initial)
    } yield (row, col)).toList
  }

  def placeObstacleAt(pos: (Int, Int), grid: List[String]): List[String] = {
    val (row, col) = pos
    grid.zipWithIndex.map {
      case (line, r) =>
        if (r == row) line.updated(col, '#')
        else line
    }
  }

  case class Guard(position: (Int, Int), direction: Char, visited: Set[(Int, Int)])
}
