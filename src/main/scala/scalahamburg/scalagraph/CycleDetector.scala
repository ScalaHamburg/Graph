package scalahamburg.scalagraph

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph
import scala.collection.Set
import scala.math.Ordered

// TODO Path benutzen
class CycleDetector[N](g: Graph[N, DiEdge], comparator:(N,N)=>Boolean) {

  def findCycles(): Set[List[N]] = {
    var visitedNodes = Set[N]()
    var cycles = Set[List[N]]()
    for (node <- g.nodes) {
      if (!visitedNodes.contains(node.value)) {
        val(newCyc, newVnodes) = deepthFirstFrom(List(node.value), visitedNodes)
         cycles = cycles ++ newCyc
         visitedNodes = visitedNodes ++ newVnodes
      }
    }
    cycles
  }

  def deepthFirstFrom(path: List[N], visitedNodes: Set[N]): (Set[List[N]], Set[N]) = {
    val start = path.first
    val successors = g.find(start).get.diSuccessors
    var cycles = Set[List[N]]();
    var newVisitedNodes = visitedNodes

    successors.foreach { n =>
      val value: N = n.value
      newVisitedNodes = newVisitedNodes + value
      if (path.contains(value)) { // loop detected
        // take only nodes in the Cycle
        val cropped = path.take(path.indexOf(value) + 1).sort(comparator)
        val firstInCycle = g.get(cropped.first)
        val nextInCycle = firstInCycle.diSuccessors.filter(n => cropped.contains(n.value))
        val cyclePath = nextInCycle.first.shortestPathTo(firstInCycle).get.nodes
        cycles = cycles + cyclePath.map(_.value)
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

    val gWithCycle = Graph("1" ~> "2") + ("2" ~> "3") + ("3" ~> "4") + ("4" ~> "5") + ("4" ~> "2")

    val cd = new CycleDetector(gWithCycle, ((a:String, b:String)=>a<b))

    println(cd.findCycles())

    val gWithMoreCycles = Graph(1 ~> 2) + (2 ~> 3) + (3 ~> 4) + (4 ~> 5) + (5 ~> 6) + (6 ~> 7) + (7 ~> 8) + (8 ~> 9) + (8 ~> 10) + (10 ~> 11) + (11 ~> 12) + (12 ~> 5) + (12 ~> 10)
    val cd2 = new CycleDetector(gWithMoreCycles,  ((a:Int, b:Int)=>a<b))
    println(cd2.findCycles())

  }
}