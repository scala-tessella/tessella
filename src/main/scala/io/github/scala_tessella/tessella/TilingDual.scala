package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Edge, EdgeOrdering, Node, NodeOrdering, isPendant}
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
    def loop(nodeToMaybeEdges: Map[Node, MaybeEdges], dualEdges: List[Edge], acc: Set[Edge], counter: Int, exclude: List[Node] = Nil): Set[Edge] =
      println(
        s"""
           |NEW CYCLE
           |exclude: $exclude
           |polygons (dual nodes) with tiling edges to be completed: $nodeToMaybeEdges
           |dual nodes [nodes ${nodeToMaybeEdges.size}]: ${nodeToMaybeEdges.keys.toList.sorted(NodeOrdering)}
           |extracted tiling edges from completed polygons [${acc.size}]: ${acc.toList.sorted(EdgeOrdering)}
           |dual edges [${dualEdges.size}]: ${dualEdges.sorted(EdgeOrdering)}
           |dual edges nodes [${dualEdges.nodes.size}]: ${dualEdges.nodes.sorted(NodeOrdering)}
           |counter for next tiling node: $counter
           |""".stripMargin)
      if nodeToMaybeEdges.isEmpty then
        acc
      else
        val polygonWithJustOneEdgeUndefined: Option[(Node, MaybeEdges)] =
          nodeToMaybeEdges.find((_, maybeEdges) => maybeEdges.count(_.isEmpty) == 1)
        polygonWithJustOneEdgeUndefined match
          case Some((dualNode, maybeEdges)) =>
            val newEdge: Edge =
              Edge(maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList)
            val adjacent: Node =
              dualEdges.nodesAdjacentTo(dualNode).head
            println(
              s"""
                 |JUST 1 EMPTY
                 |dualNode $dualNode
                 |maybeEdges $maybeEdges
                 |newEdge $newEdge
                 |adjacent $adjacent
                 |""".stripMargin)
            val set: Set[Edge] =
              Set(newEdge) ++ nodeToMaybeEdges(dualNode).flatten
            if nodeToMaybeEdges(adjacent).count(_.isEmpty) == 1 then
              println(s"#1 polygons $dualNode and $adjacent completed: both removed from map; edges from both extracted")
              loop(
                nodeToMaybeEdges.filter((node, _) => node != dualNode && node != adjacent),
                dualEdges.diff(List(Edge(dualNode, adjacent))),
                acc ++ set ++ nodeToMaybeEdges(adjacent).flatten,
                counter
              )
            else
              println(s"#2 polygon $dualNode completed: removed from map; $adjacent updated in map with shared edge $newEdge; edges from $dualNode extracted")
              loop(
                nodeToMaybeEdges.filter((node, _) => node != dualNode).updatedWith(adjacent)(replaceNoneWith(newEdge)),
                dualEdges.diff(List(Edge(dualNode, adjacent))),
                acc ++ set,
                counter
              )
          case None =>
            val polygonWithMoreCompleteRingOfEdges: Option[(Node, MaybeEdges)] =
              nodeToMaybeEdges
                .filterNot((node, _) => exclude.contains(node))
                .filter((_, maybeEdges) => maybeEdges.flatten.allDegrees.count((_, degree) => isPendant(degree)) == 2)
                .minByOption((_, maybeEdges) => maybeEdges.count(_.isEmpty))
            polygonWithMoreCompleteRingOfEdges match
              case Some((dualNode, maybeEdges)) =>
                val adjacentDualNodes: List[Node] =
                  dualEdges.nodesAdjacentTo(dualNode)
                val pendants: List[Node] =
                  maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList
                println(
                  s"""
                     |MORE THAN 1 EMPTY
                     |dualNode: $dualNode
                     |adjacentDualNodes: $adjacentDualNodes
                     |maybeEdges: $maybeEdges
                     |pendants: $pendants
                     |containing: ${dualEdges.nodesAdjacentTo(dualNode).map(nodeToMaybeEdges(_))}
                     |""".stripMargin)
//                val (pendant, adjacent): (Node, Node) =
//                  pendants.find(tilingNode =>
//                    adjacentDualNodes.exists(nodeToMaybeEdges(_).flatten.nodes.contains(tilingNode))
//                  ) match
//                    case Some(value) => (value, adjacentDualNodes.find(nodeToMaybeEdges(_).flatten.nodes.contains(value)).get)
//                    case None => ???
//                      // special case for 3.12.12 and 3.3.3.3.6 and 3.3.4.3.4
//                      adjacentDualNodes.filter(nodeToMaybeEdges(_) == List(None, None, None)) match
//                        case Nil => ???
//                        case one :: Nil => (pendants.last, one)
//                        case many =>
//                          val pippo: Node =
//                            dualEdges.nodes.find(nodeToMaybeEdges(_).flatten.nodes.contains(pendants.head)).get
//                          val adj: Node =
//                            many.minBy(start => edges.distance(start, pippo))
//                          (pendants.head, adj)
                pendants.find(tilingNode =>
                  adjacentDualNodes.exists(nodeToMaybeEdges(_).flatten.nodes.contains(tilingNode))
                ) match
                  case None => loop(nodeToMaybeEdges, dualEdges, acc, counter, dualNode :: exclude)
                  case Some(value) =>
                    val (pendant, adjacent): (Node, Node) =
                      (value, adjacentDualNodes.find(nodeToMaybeEdges(_).flatten.nodes.contains(value)).get)
                    val newEdge: Edge =
                       Edge(pendant, Node(counter))
                    println(
                        s"""
                           |pendant $pendant
                           |newEdge $newEdge
                           |""".stripMargin)
                    val addedEdges: Map[Node, MaybeEdges] =
                      nodeToMaybeEdges.updatedWith(dualNode)(replaceNoneWith(newEdge))
                    if nodeToMaybeEdges(adjacent).count(_.isEmpty) == 1 then
                      println(s"#3 polygon $adjacent completed: removed from map; $dualNode updated in map; shared edge $newEdge plus edges from $adjacent extracted")
                      loop(
                        addedEdges.filter((node, _) => node != adjacent),
                        dualEdges.diff(List(dualNode, adjacent)),
                        Set(newEdge) ++ acc ++ nodeToMaybeEdges(adjacent).flatten,
                        counter + 1
                      )
                    else
                      println(s"#4 polygons $dualNode and $adjacent both updated in map with shared edge $newEdge")
                      loop(
                        addedEdges.updatedWith(adjacent)(replaceNoneWith(newEdge)),
                        dualEdges.diff(List(Edge(dualNode, adjacent))),
                        acc,
                        counter + 1
                      )
              case None => ???

    val allEdges: Set[Edge] =
      loop(nodeToPerimeterEdges, edges.withoutNodes(boundary.toList), Set.empty[Edge], size + 1)

    Tiling.maybe(allEdges.toList)
