package scalahamburg.scalagraph

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph
import scala.collection.Set

class CycleDetector(g: Graph[Int, DiEdge]) {

  def findCycles(): List[List[Int]] = {
    var visitedNodes = Set[Int]()
    var cycles = List[List[Int]]()
    for (node <- g.nodes) {
      if (!visitedNodes.contains(node.value)) {
        deepthFirstFrom(List(node.value), visitedNodes) match {
          case (cyc, vNodes) => cycles = cycles ++ cyc; visitedNodes = visitedNodes ++ vNodes
          case _ =>
        }
      }
    }
    cycles
  }

  def deepthFirstFrom(path: List[Int], visitedNodes: Set[Int]): (List[List[Int]], Set[Int]) = {
    val start = path.first
    val successors = g.find(start).get.diSuccessors
    var cycles = List[List[Int]]();

    if (successors.isEmpty) {
      return (cycles, visitedNodes)
    }

    var newVisitedNodes = visitedNodes
    successors.foreach { n =>
      val value: Int = n.value
      newVisitedNodes = newVisitedNodes + value
      if (!visitedNodes.contains(value)) {
        if (path.contains(n.value)) {
          // loop detected
          cycles = path :: cycles
        } else {
          val newPath = n.value :: path
          val (c, newAll) = deepthFirstFrom(newPath, newVisitedNodes)
          if (!c.isEmpty) {
            cycles = cycles ++ c
          }
        }
      }
    }
    (cycles, newVisitedNodes)
  }

}

object CycleDetector {
  def main(args: Array[String]) {

    val gWithCycle = Graph(1 ~> 2) + (2 ~> 3) + (3 ~> 4) + (4 ~> 5) + (4 ~> 2)

    val cd = new CycleDetector(gWithCycle)

    println(cd.findCycles())

  }
}