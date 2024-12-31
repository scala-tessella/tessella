package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Edge, Node, isPendant}
import utility.Utils.mapValues2

/** Undirected connected graph representing the dual of a finite tessellation of unit regular polygons
 *
 * @param edges the dual graph edges
 * @param boundary the order of the 1-degree nodes
 */
class TilingDual(edges: List[Edge], boundary: Vector[Node]) extends Graph(edges):

  override def toString: String =
    s"TilingDual($boundary ${edges.stringify})"

  def polygonBoundary: Vector[Polygon] =
    boundary
      .map(edges.nodesAdjacentTo(_).head)
      .map(edges.degree)
      .map(degree => Polygon(degree.toInt))

  /** Tries to convert a [[TilingDual]] into a [[Tiling]] */
  override def toMaybeTiling: Either[String, Tiling] =

    val nodeToEmptyEdges: Map[Node, List[Option[Edge]]] =
      edges.allDegrees
        .filterNot((_, degree) => isPendant(degree))
        .mapValues2(degree => List.fill(degree.toInt)(None))

    val size: Int =
      boundary.size
    // assign perimeter edges from boundary
    val nodeToPerimeterEdges: Map[Node, List[Option[Edge]]] =
      boundary.foldLeft((nodeToEmptyEdges, 1))({ case ((map, index), boundNode) =>
        val node: Node =
          edges.nodesAdjacentTo(boundNode).head
        val newEdge: Option[Edge] =
          Option(index--(index % size + 1))
        (map.updatedWith(node)(_.map(options => newEdge :: options.init)), index + 1)
      })._1

    // tail recursive loop

    // find a node where only one edge is missing
      // add it and assign it also to the adjacent node
      // loop

    // if none, find a node where two edges are missing
      // add them with a new ordinal and assign them also to the adjacent nodes
      // loop

    // if none, exit loop

    val tilingEdges: List[Edge] =
      nodeToPerimeterEdges.values.flatten.flatten.toList
//    println(tilingEdges)
    Tiling.maybe(tilingEdges)
