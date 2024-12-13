import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.collection.{mutable => mut}

object GardenGroups2 {

  type Region = Vector[(Int, Int)]


  def cardinalPositions(x: Int, y: Int): List[(Int, Int)] = 
    List((x-1, y), (x+1, y), (x, y-1), (x, y+1))

  def neighborPositions(ix: Int, iy: Int): List[(Int, Int)] = {
    (ix -1 to ix + 1).flatMap{x =>
      (iy -1 to iy + 1).flatMap {y => 
        Option.when(x != ix || y != iy)((x, y))
      }
    }.toList
  }

  case class PlantMap(plants: Vector[String]) {
    val height: Int = plants.size
    val width: Int = plants.head.size

    def apply(x: Int, y: Int): Char = plants(y)(x)

    def isDefinedAt(x: Int, y: Int): Boolean = 
      x >= 0 && x < width && y >= 0 && y < height

    def get(x: Int, y: Int): Option[Char] = 
      Option.when(isDefinedAt(x, y))(apply(x, y))

    def floodFill(x: Int, y: Int): Region = {
      val q = Queue[(Int, Int)]()
      val char = apply(x, y)
      val res = mut.ListBuffer[(Int, Int)]()
      q.addOne((x, y))
      while (q.nonEmpty) {
        val n  = q.removeHead()
        if(get(n._1, n._2).contains(char) && !res.contains(n)) {
          res.prepend(n)
          q.addAll(cardinalPositions(n._1, n._2))
        }
      }
      res.toVector
    }

    def indices: Vector[(Int, Int)] = 
      (for {
        y <- 0 until height
        x <- 0 until width
      } yield (x, y)).toVector

    def regions: List[Region] = {
      List.unfold[Vector[(Int, Int)], Vector[(Int, Int)]](this.indices) { acc =>
        acc.headOption.map {head =>
          val points = floodFill(head._1, head._2)
          (points, acc.diff(points))
        }
      }
    }

    def optionalNeighbors(x: Int, y: Int): List[Option[Char]] = 
      neighborPositions(x, y).map(pair => get(pair._1, pair._2))
  }


  implicit class RegionSyntax(region: Region) {
    def asPlantMap: PlantMap = {
      val maxX = region.maxBy(_._1)._1
      val maxY = region.maxBy(_._2)._2
      val res = mut.ArrayBuffer.fill(maxY + 1, maxX + 1)('.')
      region.foreach { case (x, y) => 
        res(y)(x) = '#'
      }
      PlantMap(res.map(_.mkString("", "", "")).toVector)
    }

    def inflate: Region =
      region.flatMap{
        case (x, y) => List((x * 2, y * 2), (x * 2 + 1, y * 2), (x * 2, y * 2 + 1), (x * 2 + 1, y * 2 + 1))
      }
    

    def sides: Int = {
      val bigRegion = region.inflate
      val regionMap = bigRegion.asPlantMap
      bigRegion.count{ case (x, y) =>
        val neighborCount = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
        neighborCount match {
          case 3 | 4 | 7 => true
          case _ => false
        }
      }
    }

    def area: Int = region.size

  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toVector
    val plants = PlantMap(input)
    val solution = plants.regions.map(r => r.area * r.sides).sum
    println(s"Solution: $solution")
  }
}
