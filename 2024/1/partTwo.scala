import scala.io.Source

object Solution {
  def main(args: Array[String]) = {
    val input = Source.stdin.getLines()

    val (left, right) = input.map { line => 
      val parts = line.trim.split("\\s+").map(_.toInt)
      (parts(0), parts(1))
    }.toList.unzip


    val solution = countItemsIn(left, right)
    println(s"Solution: $solution")
  }

  def countItemsIn(xs: List[Int], ys: List[Int]): Int = {
    val scores: Map[Int, Int] = xs.foldLeft(Map.empty[Int, Int]){(map, x) =>
      map + (x -> ys.filter(_ == x).size)
    }

    xs.foldLeft(0){(acc, x) => 
      acc + (scores(x) * x)
    }
  }
}
