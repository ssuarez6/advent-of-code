import scala.io.Source

object CeresSearch2 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Source.fromFile("input.txt").getLines().toList
    val solution: Int = countXmasOccurrences(input)
    println(s"Solution: $solution")
  }

  def countXmasOccurrences(lines: List[String]): Int = {
    val rows = lines.length
    val cols = lines(0).length

    def matchesMAS(r1: Int, c1: Int, r2: Int, c2: Int, r3: Int, c3: Int): Boolean = {
      val chars = Seq((r1, c1), (r2, c2), (r3, c3)).map { case (r,c) => lines(r)(c)}
      chars == Seq('M', 'A', 'S') || chars == Seq('S', 'A', 'M')
    }

    def isXmasAt(r: Int, c: Int): Boolean = {
      if (r-1 < 0 || r+1 >= rows || c-1 < 0 || c+1 >= cols) false
      else {
        val topLeft = matchesMAS(r-1, c-1, r, c, r+1, c+1)
        val topRight = matchesMAS(r-1, c+1, r, c, r+1, c-1)

        topLeft && topRight
      }
    }

    var count = 0
    for {
      r <- 1 until rows - 1
      c <- 1 until cols - 1
      if isXmasAt(r, c)
    } count += 1

    count
  }
}
