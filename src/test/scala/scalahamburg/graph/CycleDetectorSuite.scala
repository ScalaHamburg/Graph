package scalahamburg.graph

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scalax.collection.{Graph => ScalaGraph}
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalahamburg.scalagraph.CycleDetector
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CycleDetectorSuite extends FunSuite with ShouldMatchers{
   test("cycleDetection on an empty Graph"){
     val empty = ScalaGraph[String, DiEdge]()
     val detector = new CycleDetector(empty, ((a:String, b:String)=>a<b))
     detector.findCycles
   }

   test("cycleDetection on a singleNode Graph"){
  	 val one = ScalaGraph[String, DiEdge]("1"~>"1")
  	 val detector = new CycleDetector(one, ((a:String, b:String)=>a<b))
  	 detector.findCycles() should equal(Set())
   }

   test("cycleDetection on a singleNode Graph with cycle"){
  	 val one = ScalaGraph[String, DiEdge]("1"~>"1") + ("1"~>"1") 
  	 val detector = new CycleDetector(one, ((a:String, b:String)=>a<b))
  	 detector.findCycles() should equal(Set())
   }
}