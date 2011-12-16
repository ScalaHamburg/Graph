package scalahamburg.scalagraph

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scalax.collection.{Graph => ScalaGraph}
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalahamburg.scalagraph.CycleDetectingGraph.gToExtG

@RunWith(classOf[JUnitRunner])
class CycleDetectorSuite extends FunSuite with ShouldMatchers{
  
   test("cycleDetection on an empty Graph"){
     val empty = ScalaGraph[String, DiEdge]()
     empty.findCycles((a: String, b: String) => a < b)
   }

   test("cycleDetection on a singleNode Graph"){
  	 val one = ScalaGraph[String, DiEdge]("1"~>"1")
  	 one.findCycles((a: String, b: String) => a < b) should equal(Set())
   }

   test("cycleDetection on a singleNode Graph with cycle"){
  	 val one = ScalaGraph[String, DiEdge]("1"~>"1") + ("1"~>"1") 
  	 one.findCycles((a: String, b: String) => a < b) should equal(Set())
   }
}