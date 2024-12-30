package io.github.scala_tessella.tessella

import Topology.{Edge, Node}

/** Methods for graph of the dual */
object TopologyDual:

  extension (tiling: Tiling)

    /** Converts a [[Tiling]] into a [[TilingDual]] */
    def toTilingDual: TilingDual =
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
      new TilingDual(perimeterDualEdges ++ internalDualEdges, perimeterDualEdges.map(_.lesserNode).toVector)

