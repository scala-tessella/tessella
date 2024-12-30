package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{Edge, Node}

/** Undirected connected graph representing the dual of a finite tessellation of unit regular polygons
 *
 * @param edges the dual graph edges
 */
class TilingDual2(edges: List[Edge]) extends Graph(edges):

  override def toString: String =
    s"TilingDual(${edges.stringify})"

  def boundarySize: Int =
    edges.pendantNodes.size

  def boundary: Vector[Polygon] =
    (1 to boundarySize)
      .map(Node(_))
      .map(edges.adjacentTo(_).head)
      .map(edges.degree)
      .map(degree => Polygon(degree.toInt))
      .toVector

  /** Tries to convert a [[TilingDual2]] into a [[Tiling]] */
  override def toMaybeTiling: Either[String, Tiling] =
    ???
