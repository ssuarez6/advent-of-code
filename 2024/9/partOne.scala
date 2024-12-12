import scala.io.Source
import scala.annotation.tailrec

object DiskFragmenter {

  def compute(input: String): BigInt = {
    val vector = input.toList.map(_ - '0').grouped(2).toVector.zipWithIndex.flatMap {
      case (List(filled, empty), i) => List.fill(filled)(Some(i)) ::: List.fill(empty)(None)
      case (List(filled), i) => List.fill(filled)(Some(i))
      case _ => List.empty
    }

    compact(vector).zipWithIndex.map(p => p._1.toLong * p._2).sum
  }

  @tailrec
  def compact(vector: Vector[Option[Int]], acc: List[Int] = List.empty): List[Int] = {
      if(vector.isEmpty) acc.reverse
      else vector.head match {
        case None if vector.tail.isEmpty => compact(Vector.empty, acc)
        case None => compact(vector.last +: vector.tail.init, acc)
        case Some(i) => compact(vector.tail, i :: acc)
      }
  }
  

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.replace("\n", "")
    val solution = compute(input)
    println(s"Solution $solution")
  }
}

