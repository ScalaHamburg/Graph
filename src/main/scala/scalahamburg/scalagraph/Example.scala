package scalahamburg.scalagraph

import scalax.collection.edge.LDiEdge
import scalax.collection.edge.LkUnDiEdge

import scalax.collection.Graph
import scalax.collection.edge.Implicits._

import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.edge.Implicits._

object Example {

  def main(args: Array[String]): Unit = {

    val g = Graph('Oradea ~ 'Zerind % 71) +
    ('Oradea ~ 'Sibiu % 151)+
    ('Zerind ~ 'Arad % 75)+
    ('Arad ~ 'Timisoara % 118)+
    ('Arad ~ 'Sibiu % 140)+
    ('Timisoara ~ 'Logoj % 111)+
    ('Logoj ~ 'Mehadia % 70)+
    ('Mehadia ~ 'Drobeta % 75)+
    ('Drobeta ~ 'Craiova % 120)+
    ('Sibiu ~ 'Rimnicu_Vilcea % 80)+
    ('Sibiu ~ 'Fagaras % 99)+
    ('Rimnicu_Vilcea ~ 'Pitesti % 97)+
    ('Rimnicu_Vilcea ~ 'Craiova % 146)+
    ('Craiova ~ 'Pitesti % 138)+
    ('Fagaras ~ 'Bucharest % 211)+
    ('Pitesti ~ 'Bucharest % 101)+
    ('Bucharest ~ 'Giurgiu % 90)+
    ('Bucharest ~ 'Urzizeni % 85)+
    ('Urzizeni ~ 'Hirsova % 98)+
    ('Urzizeni ~ 'Vaslui % 142)+
    ('Hirsova ~ 'Eforie % 86)+
    ('Vaslui ~ 'Iasi % 92)+
    ('Iasi ~ 'Neamt % 87)
    val result = g.find('Vaslui).get.shortestPathTo(g.find('Mehadia).get)
    println(result)
    println("weight: " + result.get.weight)

  }

}