package ir.usc.ac

import models.{ConnectionInfo, Graph, Node}
import algorithm.{AStarAlgorithmRunner}
import java.time.LocalTime
import java.time.temporal.ChronoUnit

object Run extends App {

  val nodes: List[Node] = Node.fromFile("C:\\\\Users\\lenovo\\Desktop\\AStarNodesInfo\\nodes.txt")

  val connectionsInfo: List[ConnectionInfo] = ConnectionInfo.fromFile("C:\\\\Users\\lenovo\\Desktop\\AStarNodesInfo\\graph.txt")

  val graph: Graph = models.Graph(nodes, connectionsInfo)

  val algorithmRunner =
    AStarAlgorithmRunner(from = "A", to = "J", graph = graph)

  lazy val path: IndexedSeq[String] = algorithmRunner.run()

  val start: LocalTime = LocalTime.now()
  path
  val finish: LocalTime = LocalTime.now()
  val totalTime: Long = ChronoUnit.MILLIS.between(start, finish)

  val indexedPath = path.drop(1).zipWithIndex.map {
    case (currentNode, lastNodeIndex) =>
      path(lastNodeIndex) -> currentNode
  }

  println {
    indexedPath.map{ traverse =>
      val (srcNode, dstNode) = traverse
      val connection = graph.connections.filter(con => con.from == srcNode && con.to == dstNode).head
      s"from `$srcNode` to `$dstNode` with weight: ${connection.weight}"
    }.mkString(", and then ") + s" in $totalTime milli seconds"
  }
}
