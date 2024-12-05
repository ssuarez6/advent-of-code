import scala.io.Source

object CeresSearch {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromFile("input.txt").getLines().toList
    val solution: Int = countWordOccurrences(input)
    println(s"Solution: $solution")
  }

  def countWordOccurrences(lines: List[String], word: String = "XMAS"): Int = {
    val rows = lines.length
    val cols = if (rows > 0) lines(0).length else 0
    val directions = Seq(
      (0, 1), //horizontal right
      (1, 0), //vertical down
      (1, 1), //diagonal down right
      (1, -1), //diagonal down left
      (0, -1), //horizontal left
      (-1, 0), //vertical up
      (-1, -1), //diagonal up left
      (-1, 1) // diagonal up right
    )

    def isWordAt(r: Int, c: Int, dr: Int, dc: Int): Boolean = {
      word.indices.forall { i => 
        val nr = r + dr * i
        val nc = c + dc * i
        nr >= 0 && nr < rows && nc >= 0 && nc < cols && lines(nr)(nc) == word(i)
      }
    }

    var count = 0
    for {
      r <- lines.indices
      c <- lines(r).indices
      (dr, dc) <- directions 
    } {
      if(isWordAt(r, c, dr, dc)) {
        count += 1
      }
    }

    count
  }
}
