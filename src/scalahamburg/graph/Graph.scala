/*
 * Copyright 2011 Hendrik Schnepel
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scalahamburg.graph

/**
 * Repräsentiert einen Pfad aus mehreren Knoten us und der Gesamtlänge weight.
 */
class Path[V](us: List[V], val weight: Long) {
  
  /**
   * Ergänzt den Pfad um einen Knoten v und die Länge w.
   */
  def add(u: V, w: Long): Path[V] = new Path(u :: us, w + weight)
  
  /**
   * Stellt den Pfad dar.
   */
  override def toString: String = (us, weight).toString
  
}

object Path {
  
  /**
   * Erzeugt einen Pfad, der nur den Knoten v enthält.
   */
  def apply[V](v: V): Path[V] = new Path(List(v), 0)
  
  /**
   * Erzeugt eine Funktion, die für eine Option auf einen Pfad entweder die Länge
   * des Pfades oder einen Default-Wert zurückgibt.
   */
  def weightOrElse[V](fallback: Long): (Option[Path[V]]) => Long = _ match {
    case Some(p) => p.weight
    case None => fallback
  }
  
}

/**
 * Repräsentiert einen ungerichteten, gewichteten Graphen.
 */
class Graph[V](g: Map[V, Map[V, Long]]) {

  /**
   * Erweitert den Graphen um die Kante e = (u, v) mit dem Gewicht w.
   */
  def add(e: (V, V), w: Long): Graph[V] = {
    val (u, v) = e;
    val n_u = g.getOrElse(u, Map())
    val n_v = g.getOrElse(v, Map())
    new Graph(g + (u -> (n_u + (v -> w)))
                + (v -> (n_v + (u -> w))))
  }
  
  /**
   * Entfernt den Knoten u und alle Kanten (u, v) aus dem Graphen.
   */
  def remove(u: V): Graph[V] = {
    val remove_u = (vs: (V, Map[V, Long])) => {
      val (v, n_v) = vs
      (v, n_v - u)
    }
    new Graph(g.map(remove_u)
               .filter(!_._2.isEmpty) - u)
  }
  
  /**
   * Gibt an, ob der Knoten in diesem Graphen Nachbarn hat, d.h. ob mindestens eine
   * Kante (u, v) existiert.
   */
  def hasNeighbours(u: V): Boolean = g.contains(u)
  
  /**
   * Gibt für einen Knoten u die Menge der Nachbarknoten n_u zurück.
   */
  def neighbours(u: V): Set[V] = g.getOrElse(u, Map()).keySet
  
  /**
   * Gibt das Gewicht der Kante e = (u, v) zurück, wenn diese Teil des Graphen ist.
   */
  def weight(e: (V, V)): Option[Long] = {
    val (u, v) = e
    g.getOrElse(u, Map()).get(v)
  }
  
  /**
   * Stellt den Graphen dar.
   */
  override def toString: String = g.toString()
  
  /**
   * Gibt den kürzesten Pfad p zwischen zwei Knoten u und destination zurück.
   */
  def shortestPath(u: V, destination: V): Option[Path[V]] =
  
    /*
     * Wenn wir uns schon am Ziel befinden, besteht der Pfad aus genau dem
     * Zielknoten.
     */
    if (u.equals(destination)) Some(Path(destination))

    /*
     * Wenn wir uns also in u noch nicht am Ziel befinden, aber u gar keine
     * Nachbarn hat, so kann es keinen (kürzesten) Pfad von u zum Ziel geben.
     */
    else if (!hasNeighbours(u)) None
    
    /*
     * Ansonsten wird per Rekursion entlang der Nachbarknoten des aktuellen Knoten
     * weitergesucht und daraus der kürzeste Pfad gewählt.
     */
    else neighbours(u).map(v => remove(u).shortestPath(v, destination) match {
        case None => None
        case Some(p) => Some(p.add(u, weight(u, v).get))
      }
    ).minBy(Path.weightOrElse(Long.MaxValue))
  
}

object Graph {
  
  /**
   * Erzeugt einen neuen, leeren Graphen.
   */
  def apply[V](): Graph[V] = new Graph[V](Map())
  
}
