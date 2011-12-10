package scalahamburg.scalagraph

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph
import scala.collection.Set

// TODO Path benutzen
// TODO Cycles "croppen"
class CycleDetector(g: Graph[Int, DiEdge]) {

  def findCycles(): Set[List[Int]] = {
    var visitedNodes = Set[Int]()
    var cycles = Set[List[Int]]()
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

  def deepthFirstFrom(path: List[Int], visitedNodes: Set[Int]): (Set[List[Int]], Set[Int]) = {
    val start = path.first
    val successors = g.find(start).get.diSuccessors
    var cycles = Set[List[Int]]();

    if (successors.isEmpty) {
      return (cycles, visitedNodes)
    }

    var newVisitedNodes = visitedNodes
    successors.foreach { n =>
      val value: Int = n.value
      newVisitedNodes = newVisitedNodes + value
        if (path.contains(value)) {
          // loop detected
          // crop
          cycles = cycles + path.take(path.indexOf(value)+1).sort(_<_)
        } else {
          val newPath = n.value :: path
          val (c, newAll) = deepthFirstFrom(newPath, newVisitedNodes)
          if (!c.isEmpty) {
            cycles = cycles ++ c
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

    val gWithMoreCycles = Graph(1 ~> 2) + (2 ~> 3) + (3 ~> 4) + (4 ~> 5) + (5 ~> 6) + (6 ~> 7) + (7 ~> 8) + (8 ~> 9) + (8 ~> 10) + (10 ~> 11) + (11 ~> 12) + (12 ~> 5) + (12 ~> 10)    
    val cd2 = new CycleDetector(gWithMoreCycles)
    println(cd2.findCycles())

  }
}