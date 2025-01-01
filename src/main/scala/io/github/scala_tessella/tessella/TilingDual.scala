package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Edge, Node, isPendant}
import utility.Utils.mapValues2

import scala.annotation.tailrec

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

    type MaybeEdges = List[Option[Edge]]

    val nodeToEmptyEdges: Map[Node, MaybeEdges] =
      edges.allDegrees
        .filterNot((_, degree) => isPendant(degree))
        .mapValues2(degree => List.fill(degree.toInt)(None))
    val size: Int =
      boundary.size
    // assign perimeter edges from boundary
    val nodeToPerimeterEdges: Map[Node, MaybeEdges] =
      boundary.foldLeft((nodeToEmptyEdges, 1))({ case ((map, index), dualNode) =>
        val adjacent: Node =
          edges.nodesAdjacentTo(dualNode).head
        val newEdge: Edge =
          index--(index % size + 1)
        (map.updatedWith(adjacent)(_.map(options => Option(newEdge) :: options.init)), index + 1)
      })._1

    def replaceNoneWith(edge: Edge): Option[MaybeEdges] => Option[MaybeEdges] =
      _.map(options =>
        val (defined, empty): (MaybeEdges, MaybeEdges) =
          options.partition(_.isDefined)
        Option(edge) :: defined ++ empty.tail
      )

    @tailrec
    def loop(map: Map[Node, MaybeEdges], dualEdges: List[Edge], counter: Int): Map[Node, MaybeEdges] =
      map.find((_, maybeEdges) => maybeEdges.count(_.isEmpty) == 1) match
        case Some((dualNode, maybeEdges)) =>
          val newEdge: Edge =
            Edge(maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList)
          val adjacent: Node =
            dualEdges.nodesAdjacentTo(dualNode).head
          val addedEdges: Map[Node, MaybeEdges] =
            map
              .updatedWith(dualNode)(_.map(_ => Option(newEdge) :: maybeEdges.filter(_.isDefined)))
              .updatedWith(adjacent)(replaceNoneWith(newEdge))
          loop(addedEdges, dualEdges.withoutNodes(List(dualNode)), counter)
        case None => map.find((_, maybeEdges) => maybeEdges.count(_.isEmpty) == 2) match
          case Some((dualNode, maybeEdges)) =>
            val pendant: Node =
              maybeEdges.flatten.allDegrees.find((_, degree) => isPendant(degree)).get._1
            val newEdge: Edge =
              Edge(pendant, Node(counter))
            val adjacent: Node =
              dualEdges.nodesAdjacentTo(dualNode).find(map(_).flatten.nodes.contains(pendant)).get
            val addedEdges: Map[Node, MaybeEdges] =
              map
                .updatedWith(dualNode)(replaceNoneWith(newEdge))
                .updatedWith(adjacent)(replaceNoneWith(newEdge))
            loop(addedEdges, dualEdges.diff(List(Edge(dualNode, adjacent))), counter + 1)
          case None => map

    val nodeToAllEdges: Map[Node, MaybeEdges] =
      loop(nodeToPerimeterEdges, edges.withoutNodes(boundary.toList), size + 1)
    Tiling.maybe(nodeToAllEdges.values.flatten.flatten.toList)
