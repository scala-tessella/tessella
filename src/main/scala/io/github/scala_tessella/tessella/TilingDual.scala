package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Edge, Node, NodeOrdering, isPendant}
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

    val dualEdges: List[Edge] =
      edges.withoutNodes(boundary.toList)

    @tailrec
    def loop(nodeToMaybeEdges: Map[Node, MaybeEdges], acc: Set[Edge], counter: Int): Set[Edge] =
//      println(
//        s"""
//           |NEW CYCLE
//           |polygons (dual nodes) with tiling edges to be completed: $nodeToMaybeEdges
//           |dual nodes [nodes ${nodeToMaybeEdges.size}]: ${nodeToMaybeEdges.keys.toList.sorted(NodeOrdering)}
//           |extracted tiling edges from completed polygons [${acc.size}]: $acc
//           |counter for next tiling node: $counter
//           |""".stripMargin)
      if nodeToMaybeEdges.isEmpty then
        acc
      else nodeToMaybeEdges.find((_, maybeEdges) => maybeEdges.count(_.isEmpty) == 1) match
        case Some((dualNode, maybeEdges)) =>
          val newEdge: Edge =
            Edge(maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList)
          val adjacent: Node =
            dualEdges.nodesAdjacentTo(dualNode).intersect(nodeToMaybeEdges.keys.toList).head
//          println(
//            s"""
//               |JUST 1 EMPTY
//               |dualNode $dualNode
//               |maybeEdges $maybeEdges
//               |newEdge $newEdge
//               |adjacent $adjacent
//               |""".stripMargin)
          val set: Set[Edge] =
            Set(newEdge) ++ nodeToMaybeEdges(dualNode).flatten
          if nodeToMaybeEdges(adjacent).count(_.isEmpty) == 1 then
//            println(s"#1 polygons $dualNode and $adjacent completed: removed from map; edges from both extracted")
            loop(nodeToMaybeEdges.filter((node, _) => node != dualNode && node != adjacent), acc ++ set ++ nodeToMaybeEdges(adjacent).flatten, counter)
          else
//            println(s"#2 polygon $dualNode completed: removed from map; $adjacent updated in map; edges from $dualNode extracted")
            loop(nodeToMaybeEdges.filter((node, _) => node != dualNode).updatedWith(adjacent)(replaceNoneWith(newEdge)), acc ++ set, counter)
        case None => nodeToMaybeEdges.filter((_, maybeEdges) => maybeEdges.exists(_.isDefined)).minByOption((_, maybeEdges) => maybeEdges.count(_.isEmpty)) match
          case Some((dualNode, maybeEdges)) =>
//            println(
//              s"""
//                 |MORE THAN 1 EMPTY
//                 |dualNode $dualNode
//                 |maybeEdges $maybeEdges
//                 |""".stripMargin)
            val pendant: Node =
              maybeEdges.flatten.allDegrees.find((_, degree) => isPendant(degree)).get._1
            val newEdge: Edge =
              Edge(pendant, Node(counter))
//            println(
//              s"""
//                 |pendant $pendant
//                 |newEdge $newEdge
//                 |adjacents: ${dualEdges.nodesAdjacentTo(dualNode)}
//                 |adj2: ${dualEdges.nodesAdjacentTo(dualNode).map(x => nodeToMaybeEdges(x))}
//                 |""".stripMargin)
            val adjacent: Node =
              dualEdges.nodesAdjacentTo(dualNode).intersect(nodeToMaybeEdges.keys.toList).find(nodeToMaybeEdges(_).flatten.nodes.contains(pendant)).get
//            println(
//              s"""
//                 |adjacent $adjacent
//                 |""".stripMargin)
            val addedEdges: Map[Node, MaybeEdges] =
              nodeToMaybeEdges.updatedWith(dualNode)(replaceNoneWith(newEdge))
            if nodeToMaybeEdges(adjacent).count(_.isEmpty) == 1 then
//              println(s"#3 polygon $adjacent completed: removed from map; $dualNode updated in map; edges from $adjacent extracted")
              loop(addedEdges.filter((node, _) => node != adjacent), Set(newEdge) ++ acc ++ nodeToMaybeEdges(adjacent).flatten, counter + 1)
            else
//              println(s"#4 polygons $dualNode and $adjacent updated in map")
              loop(addedEdges.updatedWith(adjacent)(replaceNoneWith(newEdge)), acc, counter + 1)
          case None => ???

    val allEdges: Set[Edge] =
      loop(nodeToPerimeterEdges, Set.empty[Edge], size + 1)

    Tiling.maybe(allEdges.toList)
