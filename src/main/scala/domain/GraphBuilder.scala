package com.chrisworks
package domain

case class GraphBuilder(relationships: List[Graph] = List()) {

  /** Our hope here is to build a digraph string which we hope to pass to GraphViz cli to produce us a graph
   *
   * e.g:
   * """
   *  digraph {
   *      startNode -> List[OtherNodes]
   *      for (graph : relationships) {
   *        graph.name -> List[graph.directions]
   *      }
   *  }
   * """
   *
   *
   * */
  def buildGraph(): String = relationships
    .flatMap(g => g.directions.map(d => GraphBuilder.buildDirection(g.name, d)))
    .mkString("digraph ProfileSync {\n\t", "\n\t", "\n}")

}

object GraphBuilder {

  def buildDirection(data: String*): String = data.mkString(" -> ")
  def buildDirection(key: String, data: List[String]): String = (key +: data).mkString(" -> ")
}
