package ir.usc.ac
package algorithm

import models.Graph

import scala.annotation.tailrec

case class AStarAlgorithmRunner(from: String, to: String, graph: Graph) {

  object EdgeOrdering extends Ordering[(String, Long)] {
    override def compare(x: (String, Long), y: (String, Long)): Int = x._2 compare y._2
  }

  @tailrec
  private def helper(path: IndexedSeq[String], lastNode: String, previousNode: Option[String]): IndexedSeq[String] = {
    val lastNodeConnections = if (path.isEmpty) {
      graph.findSpecificNodeConnections(lastNode)
    } else {
      graph.findSpecificNodeConnections(lastNode).filterNot(c => c.from == previousNode.get || c.to == previousNode.get)
    }
    val a = lastNodeConnections.flatMap { connection =>
      val otherNode = if (connection.from == lastNode) {
        connection.to
      } else {
        connection.from
      }
      val destinationNode = graph.getNodeByName(otherNode)
      destinationNode.map { destination =>
        val weight = destination.heuristic + connection.weight
        (destination.name, weight)
      }
    }
    val minimumWeightedDestination = a.min(EdgeOrdering)
    if (minimumWeightedDestination._1 == to) {
      path.appended(minimumWeightedDestination._1)
    } else {
      helper(path.appended(minimumWeightedDestination._1), minimumWeightedDestination._1, Some(lastNode))
    }
  }

  def run(): IndexedSeq[String] = {
    IndexedSeq(from).appendedAll(helper(IndexedSeq(), from, None))
  }
}
