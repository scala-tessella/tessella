package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{Edge, Node}

/** Undirected connected graph representing the dual of a finite tessellation of unit regular polygons
 *
 * @param edges the dual graph edges
 */
class TilingDual2(edges: List[Edge], boundary: Vector[Node]) extends Graph(edges):

  override def toString: String =
    s"TilingDual($boundary ${edges.stringify})"

  def polygonBoundary: Vector[Polygon] =
    boundary
      .map(edges.adjacentTo(_).head)
      .map(edges.degree)
      .map(degree => Polygon(degree.toInt))

  /** Tries to convert a [[TilingDual2]] into a [[Tiling]] */
  override def toMaybeTiling: Either[String, Tiling] =
    ???
