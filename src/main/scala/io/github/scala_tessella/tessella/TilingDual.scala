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
    s"TilingDual(${nodes.map(node => s"${node._1} p${node._2}")}, ${edges.stringify})"

  /** Tries to convert a [[TilingDual]] into a [[Tiling]] */
  def toMaybeTiling: Either[String, Tiling] =
    ???
