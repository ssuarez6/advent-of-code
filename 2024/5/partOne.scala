import scala.io.Source

object PrintQueue { 

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    
    val (rules, updates) = buildRulesAndUpdates(input)

    val solution: Int = updates.filter(isValid(rules)).map(u => u(u.size/2)).sum

    println(s"Solution $solution")
  }

  def buildRulesAndUpdates(input: String): (Map[Int, List[Int]], List[List[Int]]) = {
    val rulesRegex = """(\d+)\|(\d+)""".r
    val Array(rulesStr, updatesStr) = input.split("\n\n")
    val rules: Map[Int, List[Int]] = rulesRegex.findAllMatchIn(rulesStr).map { m =>
      m.group(1).toInt -> m.group(2).toInt
    }.toList.groupMap(_._1)(_._2)
    val updates: List[List[Int]] = updatesStr.linesIterator.map(_.split(",").map(_.toInt).toList).toList
    (rules, updates)
  }

  def isValid(rules: Map[Int, List[Int]])(updates: List[Int]): Boolean = {
    def helper(update: List[Int], visited: Set[Int]): Boolean = update match {
      case Nil => true
      case updateN :: tail => 
        !rules.getOrElse(updateN, List.empty).exists(visited.contains) && 
        helper(tail, visited + updateN)
    }
    helper(updates, Set.empty)
  }

}
