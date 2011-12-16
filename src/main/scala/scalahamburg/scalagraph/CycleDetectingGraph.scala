package scalahamburg.scalagraph

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.Graph
import scala.collection.Set
import scala.math.Ordered
import scalax.collection.GraphTraversal
import scalax.collection.GraphTraversalImpl
import scala.collection.mutable.ListBuffer

// TODO Path benutzen

final class CycleDetectingGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N, E]) {

  /**
   * Sucht Zyklen in einem gerichteten Graphen.
   */
  def findCycles(comparator: (N, N) => Boolean): Set[g.Path] = {
    var visitedNodes = Set[N]()
    var cycles = Set[g.Path]()
    for (node <- g.nodes) {
      if (!visitedNodes.contains(node.value)) {
        val (newCyc, newVnodes) = deepthFirstFrom(List(node.value), visitedNodes, comparator)
        cycles = cycles ++ newCyc
        visitedNodes = visitedNodes ++ newVnodes
      }
    }
    val buff = new ListBuffer[N]()
    cycles.filter{
      c=>if(buff.contains(c.nodes.first.value)){false}else{buff+=c.nodes.first.value;true}
    }
  }

  /**
   * Sucht in einem gerichteten Graphen einen TeilGraphen von einem 
   * Knoten bis zum Ende oder bis ein Zyklus entdeckt wird. 
   */
  def deepthFirstFrom(path: List[N], visitedNodes: Set[N], comparator: (N, N) => Boolean): (Set[g.Path], Set[N]) = {
    val start = path.first
    val successors = g.find(start).get.diSuccessors
    var cycles = Set[g.Path]();
    var newVisitedNodes = visitedNodes

    successors.foreach { n =>
      val value: N = n.value
      newVisitedNodes = newVisitedNodes + value
      if (path.contains(value)) { // loop detected
        // take only nodes in the Cycle
        val cropped = path.take(path.indexOf(value) + 1).sort(comparator)
        val firstInCycle = g.get(cropped.first)
        val nextInCycle = firstInCycle.diSuccessors.filter(n => cropped.contains(n.value))
        val cyclePath = nextInCycle.first.shortestPathTo(firstInCycle).get
        cycles = cycles + cyclePath
      } else {
        val newPath = n.value :: path
        val (c, newAll) = deepthFirstFrom(newPath, newVisitedNodes, comparator)
        if (!c.isEmpty) {
          cycles = cycles ++ c
        }
      }
    }
    (cycles, newVisitedNodes)
  }
}

object CycleDetectingGraph {

  implicit def gToExtG[N, E[X] <: EdgeLikeIn[X]](g: Graph[N, E]) = new CycleDetectingGraph[N, E](g)
  
  def main(args: Array[String]) {

    val gWithCycle = Graph("1" ~> "2") + ("2" ~> "3") + ("3" ~> "4") + ("4" ~> "5") + ("4" ~> "2")
    //    val cd = new CycleDetector(gWithCycle, ((a: String, b: String) => a < b))
    println(gWithCycle.findCycles((a: String, b: String) => a < b))

    val gWithMoreCycles = Graph(1 ~> 2) + (2 ~> 3) + (3 ~> 4) + (4 ~> 5) + (5 ~> 6) + (6 ~> 7) + (7 ~> 8) + (8 ~> 9) + (8 ~> 10) + (10 ~> 11) + (11 ~> 12) + (12 ~> 5) + (12 ~> 10)
    println(gWithMoreCycles.findCycles((a:Int, b:Int)=>a<b))

  }
}