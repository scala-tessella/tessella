package io.github.scala_tessella.tessella

import Topology.Edge
import TopologyDual.NodeDual

/** Undirected connected graph representing the dual of a finite tessellation of unit regular polygons
 *
 * @param nodes the dual graph nodes
 * @param edges the dual graph edges
 */
class TilingDual(nodes: List[NodeDual], edges: List[Edge]):

  override def toString: String =
    val nodesString: Option[String] =
      Option(s"(${nodes.map(node => s"${node._1} p${node._2}").mkString(", ")})")
    val edgesMaybeString: Option[String] =
      if edges.isEmpty then None else Option(edges.stringify)
    s"TilingDual(${List(nodesString, edgesMaybeString).flatten.mkString(", ")})"

  /** Tries to convert a [[TilingDual]] into a [[Tiling]] */
  def toMaybeTiling: Either[String, Tiling] =
    ???
