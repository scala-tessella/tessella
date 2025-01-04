package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Edge, EdgeOrdering, Node, NodeOrdering, isPendant}
import utility.Utils.mapValues2

import io.github.scala_tessella.ring_seq.RingSeq.slidingO

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

  def minDistanceToBoundary(node: Node): Int =
    if boundary.contains(node) then 0
    else boundary.map(boundaryNode => edges.distance(node, boundaryNode)).min

  lazy val distances: Map[Node, Int] =
    edges.nodes.diff(boundary).map(node => node -> minDistanceToBoundary(node)).toMap

  /** Tries to convert a [[TilingDual]] into a [[Tiling]] */
  def toMaybeTiling2: Either[String, Tiling] =
    type TNode = Node
    type TEdge = Edge

    val boundaryEdges: List[Edge] =
      boundary.slidingO(2).toList.map(Edge(_))
    val inflatedGraph: List[Edge] =
      boundaryEdges ++ edges

    def extractPolygonsFromGraph(graph: List[Edge]): List[List[Edge]] =
      ???

    val dualPolygons: List[List[Edge]] =
      extractPolygonsFromGraph(inflatedGraph)

    val tNodeToEdges: Map[TNode, List[Edge]] =
      dualPolygons.zipWithIndex.map((edges, index) => Node(index + 1) -> edges).toMap

    val nodeToTNodes: Map[Node, List[TNode]] =
      edges.nodes.map(node => node -> tNodeToEdges.filter((_, edgez) => edgez.nodes.contains(node)).keys.toList).toMap

    def assembleTEdges(map: Map[Node, List[TNode]]): List[TEdge] =
      ???

    Tiling.maybe(assembleTEdges(nodeToTNodes))

  /** Tries to convert a [[TilingDual]] into a [[Tiling]] */
  override def toMaybeTiling: Either[String, Tiling] =
    // just aliases to visually distinguish nodes and edges of the tiling
    type TNode = Node
    type TEdge = Edge
    type MaybeEdges = List[Option[TEdge]]
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
        val newEdge: TEdge =
          index--(index % size + 1)
        (map.updatedWith(adjacent)(_.map(options => Option(newEdge) :: options.init)), index + 1)
      })._1

    def replaceNoneWith(edge: TEdge): Option[MaybeEdges] => Option[MaybeEdges] =
      _.map(options =>
        val (defined, empty): (MaybeEdges, MaybeEdges) =
          options.partition(_.isDefined)
        Option(edge) :: defined ++ empty.tail
      )

    @tailrec
    def loop(nodeToMaybeEdges: Map[Node, MaybeEdges], dualEdges: List[Edge], acc: Set[TEdge], counter: Int, exclude: List[Node] = Nil): Set[TEdge] =
//      println(
//        s"""
//           |NEW CYCLE
//           |exclude: $exclude
//           |polygons (dual nodes) with tiling edges to be completed: $nodeToMaybeEdges
//           |dual nodes [nodes ${nodeToMaybeEdges.size}]: ${nodeToMaybeEdges.keys.toList.sorted(NodeOrdering)}
//           |extracted tiling edges from completed polygons [${acc.size}]: ${acc.toList.sorted(EdgeOrdering)}
//           |dual edges [${dualEdges.size}]: ${dualEdges.sorted(EdgeOrdering)}
//           |dual edges nodes [${dualEdges.nodes.size}]: ${dualEdges.nodes.sorted(NodeOrdering)}
//           |counter for next tiling node: $counter
//           |""".stripMargin)
      if nodeToMaybeEdges.isEmpty then
        acc
      else
        val polygonWithJustOneEdgeUndefined: Option[(Node, MaybeEdges)] =
          nodeToMaybeEdges.find((_, maybeEdges) => maybeEdges.count(_.isEmpty) == 1)
        polygonWithJustOneEdgeUndefined match
          case Some((dualNode, maybeEdges)) =>
            val newEdge: TEdge =
              Edge(maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList)
            val adjacent: Node =
              dualEdges.nodesAdjacentTo(dualNode).head
//            println(
//              s"""
//                 |JUST 1 EMPTY
//                 |dualNode $dualNode
//                 |maybeEdges $maybeEdges
//                 |newEdge $newEdge
//                 |adjacent $adjacent
//                 |""".stripMargin)
            val set: Set[TEdge] =
              Set(newEdge) ++ nodeToMaybeEdges(dualNode).flatten
            if nodeToMaybeEdges(adjacent).count(_.isEmpty) == 1 then
              println(s"#1 polygons $dualNode and $adjacent completed: both removed from map; set updated with new edge $newEdge; edges from both extracted")
              loop(
                nodeToMaybeEdges.filter((node, _) => node != dualNode && node != adjacent),
                dualEdges.diff(List(Edge(dualNode, adjacent))),
                acc ++ set ++ nodeToMaybeEdges(adjacent).flatten,
                counter
              )
            else
              println(s"#2 polygon $dualNode [p${nodeToMaybeEdges(dualNode).size}] completed: removed from map; $adjacent [p${nodeToMaybeEdges(adjacent).size}] updated in map with shared edge $newEdge; edges from $dualNode extracted")
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
                val pendants: List[TNode] =
                  maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList
//                println(
//                  s"""
//                     |MORE THAN 1 EMPTY
//                     |dualNode: $dualNode
//                     |adjacentDualNodes: $adjacentDualNodes
//                     |maybeEdges: $maybeEdges
//                     |pendants: $pendants
//                     |containing: ${dualEdges.nodesAdjacentTo(dualNode).map(nodeToMaybeEdges(_))}
//                     |""".stripMargin)
                pendants.find(tilingNode =>
                  adjacentDualNodes.exists(nodeToMaybeEdges(_).flatten.nodes.contains(tilingNode))
                ) match
                  case None => loop(nodeToMaybeEdges, dualEdges, acc, counter, dualNode :: exclude)
                  case Some(value) =>
                    val (pendant, adjacent): (TNode, Node) =
                      (value, adjacentDualNodes.find(nodeToMaybeEdges(_).flatten.nodes.contains(value)).get)
                    val newEdge: TEdge =
                       Edge(pendant, Node(counter))
//                    println(
//                        s"""
//                           |pendant $pendant
//                           |newEdge $newEdge
//                           |""".stripMargin)
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
                      println(s"#4 polygons $dualNode [p${nodeToMaybeEdges(dualNode).size}] and $adjacent [p${nodeToMaybeEdges(adjacent).size}] both updated in map with shared edge $newEdge")
                      loop(
                        addedEdges.updatedWith(adjacent)(replaceNoneWith(newEdge)),
                        dualEdges.diff(List(Edge(dualNode, adjacent))),
                        acc,
                        counter + 1
                      )
              case None =>
//                println(
//                  s"""
//                     |exclude: $exclude
//                     |adjacents: ${exclude.map(node => dualEdges.nodesAdjacentTo(node).map(nodeToMaybeEdges)).mkString("\n")}
//                     |pendants: ${exclude.map(dualNode => nodeToMaybeEdges(dualNode).flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList)}
//                     |""".stripMargin)
//                exclude.sortBy(distances).headOption match
//                exclude.maxByOption(dualNode => dualEdges.nodesAdjacentTo(dualNode).size) match
//                exclude.find(dualNode => dualEdges.nodesAdjacentTo(dualNode).map(nodeToMaybeEdges(_)).contains(List(None, None, None))) match
                exclude.headOption match
                  case None => ???
                  // special cases found in 3.6.3.6 and 3.12.12
                  case Some(dualNode) =>
                    val maybeEdges: MaybeEdges =
                      nodeToMaybeEdges(exclude.head)
                    val adjacentDualNodes: List[Node] =
                      dualEdges.nodesAdjacentTo(dualNode)
                    val pendants: List[TNode] =
                      maybeEdges.flatten.allDegrees.filter((_, degree) => isPendant(degree)).keys.toList
//                    println(
//                      s"""
//                         |SPECIAL
//                         |dualNode: $dualNode
//                         |adjacentDualNodes: $adjacentDualNodes
//                         |maybeEdges: $maybeEdges
//                         |pendants: $pendants
//                         |containing: ${dualEdges.nodesAdjacentTo(dualNode).map(nodeToMaybeEdges(_))}
//                         |""".stripMargin)
                    val pendant: TNode =
                      pendants.head
                    val pendantAlt: TNode =
                      pendants.last
                    val emptyTriangle: MaybeEdges =
                      List(None, None, None)
                    adjacentDualNodes.filter(nodeToMaybeEdges(_) == emptyTriangle) match
                      case Nil =>
                        println(
                          s"""
                             |SPECIAL
                             |dualNode: $dualNode
                             |adjacentDualNodes: $adjacentDualNodes
                             |maybeEdges: $maybeEdges
                             |pendants: $pendants
                             |containing: ${dualEdges.nodesAdjacentTo(dualNode).map(nodeToMaybeEdges(_))}
                             |""".stripMargin)

                        ???
//                      case one :: Nil => ???
                      case many =>
                        val maybeAnchors: List[Option[Node]] =
                          pendants.map(pendant => dualEdges.nodes.filterNot(_ == dualNode).find(nodeToMaybeEdges(_).flatten.nodes.contains(pendant)))
                        val anchor: Node =
                          maybeAnchors.head.getOrElse(maybeAnchors.last.get)
//                          dualEdges.nodes.filterNot(_ == dualNode).find(nodeToMaybeEdges(_).flatten.nodes.contains(pendant)).get
                        val anchorAlt: Node =
                          maybeAnchors.last.getOrElse(maybeAnchors.head.get)
//                          dualEdges.nodes.filterNot(_ == dualNode).find(nodeToMaybeEdges(_).flatten.nodes.contains(pendantAlt)).getOrElse(anchor)
                        val adjacent: Node =
                          many.minBy(edges.distance(_, anchor))
                        val adjacentAlt: Node =
                          many.minBy(edges.distance(_, anchorAlt))
                        val distances: List[(Node, Int)] =
                          many.map(node => node -> edges.distance(node, anchor)).sortBy((_, distance) => distance)
                        val distancesAlt: List[(Node, Int)] =
                          many.map(node => node -> edges.distance(node, anchorAlt)).sortBy((_, distance) => distance)
                        println(s"""distances: $distances""")
                        println(s"""alt distances: $distancesAlt""")
                        val isAlt: Boolean =
//                          false
                          distancesAlt.map(_._2).sum < distances.map(_._2).sum
//                        println(
//                          s"""
//                             |pendant: $pendant
//                             |anchor: $anchor
//                             |edge to be removed: $dualNode--$adjacent
//                             |""".stripMargin)

                        val newEdge: TEdge =
                          Edge(pendant, Node(counter))
                        val newEdgeAlt: TEdge =
                          Edge(pendantAlt, Node(counter))
                        val newEdgeFinal: TEdge =
                          if isAlt then newEdgeAlt else newEdge
                        val adjacentFinal: Node =
                          if isAlt then adjacentAlt else adjacent
                        val pendantFinal: TNode =
                          if isAlt then pendantAlt else pendant
                        val addedEdges: Map[Node, MaybeEdges] =
                          nodeToMaybeEdges.updatedWith(dualNode)(replaceNoneWith(newEdge))
                        val addedEdgesAlt: Map[Node, MaybeEdges] =
                          nodeToMaybeEdges.updatedWith(dualNode)(replaceNoneWith(newEdgeAlt))
                        val addedEdgesFinal: Map[Node, MaybeEdges] =
                          nodeToMaybeEdges.updatedWith(dualNode)(replaceNoneWith(newEdgeFinal))
                        println(s"#5 [many:${many.size}] polygons $dualNode and $adjacentFinal both updated in map with shared edge $newEdgeFinal")
                        loop(
                          addedEdgesFinal.updatedWith(adjacentFinal)(replaceNoneWith(newEdgeFinal)),
                          dualEdges.diff(List(Edge(dualNode, adjacentFinal))),
                          acc,
                          counter + 1
                        )

    val allEdges: Set[TEdge] =
      if nodeToPerimeterEdges.sizeIs == 1 then
        // one polygon tiling
        nodeToPerimeterEdges.values.flatten.flatten.toSet
      else
        loop(nodeToPerimeterEdges, edges.withoutNodes(boundary.toList), Set.empty[TEdge], size + 1)
    Tiling.maybe(allEdges.toList)
