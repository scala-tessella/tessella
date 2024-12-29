package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{Edge, Node}

/** Methods for graph of the dual */
object TopologyDual:

  /** Graph node of the dual */
  type NodeDual = (Node, Polygon)

  extension (tiling: Tiling)

    /** Converts a [[Tiling]] into a [[TilingDual]] */
    def toTilingDual: TilingDual =
      val nodesMap: Map[List[Edge], NodeDual] =
        tiling.orientedPolygons.zipWithIndex.map(
          (polygonPath, ordinal) => polygonPath.toPolygonEdges -> (Node(ordinal + 1), polygonPath.toPolygon)
        ).toMap
      val polygonEdges: List[List[Edge]] =
        nodesMap.keys.toList
      val edges: List[Edge] =
        tiling.nonPerimeterEdges.map(edge =>
          Edge(polygonEdges.filter(_.contains(edge)).map(nodesMap(_)._1))
        )
      new TilingDual(nodesMap.values.toList, edges)
