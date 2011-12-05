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

object ShortestPathDemo extends Application {
  
  val empty = Graph()
  
  val simple = Graph()
    .add(("Kiel(SH)", 'Hamburg), 100)
    .add(('Hamburg, 'Bremen), 120)
    .add(('Hamburg, 'Berlin), 300)
    
  simple.add((0.815f, 4711), 1)
  
  Console.println(simple)
  Console.println()
  
  val romania = Graph()
    .add(('Oradea, 'Zerind), 71)
    .add(('Oradea, 'Sibiu), 151)
    .add(('Zerind, 'Arad), 75)
    .add(('Arad, 'Timisoara), 118)
    .add(('Arad, 'Sibiu), 140)
    .add(('Timisoara, 'Logoj), 111)
    .add(('Logoj, 'Mehadia), 70)
    .add(('Mehadia, 'Drobeta), 75)
    .add(('Drobeta, 'Craiova), 120)
    .add(('Sibiu, 'Rimnicu_Vilcea), 80)
    .add(('Sibiu, 'Fagaras), 99)
    .add(('Rimnicu_Vilcea, 'Pitesti), 97)
    .add(('Rimnicu_Vilcea, 'Craiova), 146)
    .add(('Craiova, 'Pitesti), 138)
    .add(('Fagaras, 'Bucharest), 211)
    .add(('Pitesti, 'Bucharest), 101 )
    .add(('Bucharest, 'Giurgiu), 90)
    .add(('Bucharest, 'Urzizeni), 85)
    .add(('Urzizeni, 'Hirsova), 98)
    .add(('Urzizeni, 'Vaslui), 142)
    .add(('Hirsova, 'Eforie), 86)
    .add(('Vaslui, 'Iasi), 92)
    .add(('Iasi, 'Neamt), 87)

  Console.println(romania.shortestPath('Suharu, 'Vaslui))
  Console.println(romania.shortestPath('Suharu, 'Suharu))
  Console.println(romania.shortestPath('Vaslui, 'Mehadia))
  Console.println()
  
}
