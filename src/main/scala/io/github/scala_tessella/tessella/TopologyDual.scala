package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{Edge, Node}

/** Methods for graph of the dual */
object TopologyDual:

  /** Graph node of the dual */
  type NodeDual = (Node, Option[Polygon])

  extension (tiling: Tiling)

    /** Converts a [[Tiling]] into a [[TilingDual]] */
    def toTilingDual: TilingDual =
      val perimeterEdges: List[Edge] =
        tiling.perimeter.toRingEdges.toList
      val perimeterSize: Int =
        perimeterEdges.size
      val nodesMap: Map[List[Edge], NodeDual] =
        tiling.orientedPolygons.zipWithIndex.map(
          (polygonPath, ordinal) =>
            polygonPath.toPolygonEdges -> (Node(ordinal + perimeterSize + 1), Option(polygonPath.toPolygon))
        ).toMap
      val polygonEdges: List[List[Edge]] =
        nodesMap.keys.toList
      val internalDualEdges: List[Edge] =
        tiling.nonPerimeterEdges.map(edge =>
          Edge(polygonEdges.filter(_.contains(edge)).map(nodesMap(_)._1))
        )
      val perimeterDual: List[(NodeDual, Edge)] =
        perimeterEdges.indices.map(index =>
          val node: Node =
            Node(index + 1)
          ((node, None), Edge(node, nodesMap(polygonEdges.find(_.contains(perimeterEdges(index))).get)._1)
        )).toList
      new TilingDual(
        perimeterDual.map(_._1) ++ nodesMap.values.toList,
        perimeterDual.map(_._2) ++ internalDualEdges
      )

    /** Converts a [[Tiling]] into a [[TilingDual2]] */
    def toTilingDual2: TilingDual2 =
      val perimeterEdges: List[Edge] =
        tiling.perimeter.toRingEdges.toList
      val perimeterSize: Int =
        perimeterEdges.size
      val nodesMap: Map[List[Edge], Node] =
        tiling.orientedPolygons.zipWithIndex.map(
          (polygonPath, ordinal) =>
            polygonPath.toPolygonEdges -> Node(ordinal + perimeterSize + 1)
        ).toMap
      val polygonEdges: List[List[Edge]] =
        nodesMap.keys.toList
      val internalDualEdges: List[Edge] =
        tiling.nonPerimeterEdges.map(edge =>
          Edge(polygonEdges.filter(_.contains(edge)).map(nodesMap(_)))
        )
      val perimeterDualEdges: List[Edge] =
        perimeterEdges.indices.map(index =>
          Edge(Node(index + 1), nodesMap(polygonEdges.find(_.contains(perimeterEdges(index))).get))
        ).toList
      new TilingDual2(perimeterDualEdges ++ internalDualEdges, perimeterDualEdges.map(_.lesserNode).toVector)

