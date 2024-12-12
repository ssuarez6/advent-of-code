import scala.io.Source
import scala.annotation.tailrec

object DiskFragmenter2 {

  case class Block(size: Int, value: Option[Int]) {
    def replace(b: Block): List[Block] = if (b.size < size) List(b, Block(size - b.size, value)) else List(b)
  }

  def compute(input: String): BigInt = {
    val blocks = input.toList.map(_ - '0').grouped(2).toVector.zipWithIndex.flatMap {
      case (List(filled, empty), i) => List(Block(filled, Some(i)), Block(empty, None))
      case (List(filled), i) => List(Block(filled, Some(i)))
    }

    val spaceCells = compact(blocks, List.empty).flatMap(b => Vector.fill(b.size)(b.value.getOrElse(0)))
    spaceCells.zipWithIndex.map(p => p._1.toLong * p._2).sum
  }

  @tailrec
  def compact(blocks: Vector[Block], acc: List[Block] = List.empty): List[Block] = {
    blocks.lastOption match {
      case None => acc
      case Some(last) if last.value.isEmpty => compact(blocks.init, last :: acc)
      case Some(last) => 
        blocks.indices.init.find(i => blocks(i).value.isEmpty && blocks(i).size >= last.size) match {
          case None => compact(blocks.init, last :: acc)
          case Some(ind) => 
            val replaced = blocks.take(ind) ++ blocks(ind).replace(last) ++ blocks.drop(ind + 1).init
            compact(replaced, Block(last.size, None) :: acc)
        }
    }
  }
  

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.replace("\n", "")
    val solution = compute(input)
    println(s"Solution $solution")
  }
}

