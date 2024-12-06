import scala.io.Source
import scala.collection.immutable.Queue

object PrintQueue2 { 

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    
    val (rules, updates) = buildRulesAndUpdates(input)

    val invalidUpdates = updates.filter(!isValid(rules)(_))

    val solution = invalidUpdates.map(iu => correctUpdate(iu, rules)).map(u => u(u.size/2)).sum

    println(s"Solution: $solution")
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

  def correctUpdate(update: List[Int], rules: Map[Int, List[Int]]): List[Int] = {
    val relevantRules = rules
      .filter{case (k, vs) => update.contains(k) && vs.exists(update.contains)}
      .mapValues(_.filter(update.contains)).toMap
    val prevsMap = relevantRules
      .map{ case (k, vs) => vs.map(_ -> k)}
      .flatten.groupMap(_._1)(_._2)
    val startNodes = update.filter(k => !relevantRules.values.flatten.toList.contains(k))

    def bfs(queue: Queue[Int], visited: Set[Int], res: List[Int]): List[Int] = queue.dequeueOption match {
      case None => res
      case Some((node, queueTail)) => {
        val newVisited = visited + node
        val newRes = res :+ node
        val newQ = relevantRules.getOrElse(node, List.empty)
          .filter { n => 
            val notVisited = !newVisited.contains(n)
            val notInQueue = !queueTail.contains(n)
            val allPrevVisited = prevsMap.getOrElse(n, List.empty).forall(p => newVisited.contains(p) || queueTail.contains(p))
            notVisited && notInQueue && allPrevVisited
          }
          .foldLeft(queueTail)(_.appended(_))
          bfs(newQ, newVisited, newRes)
      }
    }

    bfs(Queue.from(startNodes), Set.empty, List.empty)
  }

}
