package io.github.scala_tessella.tessella
package conversion

import ConverterDOT.Style
import Topology.{Edge, Node}

/** Methods to convert a `Graph` into a DOT file */
object DOT extends ConverterDOT:

  /** Edge styled with highlighted `color` */
  val highEdgeStyle: Style =
    Style(color("red"))

  /** Node styled with highlighted `color` and `fontcolor` */
  val highNodeStyle: Style =
    Style(List(color("red"), fontcolor("red")) *)

  extension (graph: Graph)

    /** Creates a DOT representation of a graph, with optional highlighting
     * 
     * @param highlightedNodes nodes to highlight
     * @param highlightedEdges edges to highlight
     */
    def toDOT(highlightedNodes: List[Node] = Nil, highlightedEdges: List[Edge] = Nil): String =
      val edges: List[(String, String, Style)] =
        graph.graphEdges.map(edge => (
          edge.lesserNode.toString,
          edge.greaterNode.toString,
          if highlightedEdges.contains(edge) then highEdgeStyle else Style(Nil *)
        ))
      val nodes: List[(String, Style)] =
        highlightedNodes.map(node => (node.toString, highNodeStyle))
      this.graph(edges, nodes)
