package io.github.scala_tessella.tessella

import Topology.{Edge, Node}

/** Methods for graph of the dual */
object TopologyDual:

  extension (tiling: Tiling)

    /** Converts a [[Tiling]] into a [[TilingDual]] */
    def toTilingDual: TilingDual =
      // just aliases to visually distinguish nodes and edges of the dual
      type DNode = Node
      type DEdge = Edge
      val perimeterEdges: List[Edge] =
        tiling.perimeter.toRingEdges.toList
      val perimeterSize: Int =
        perimeterEdges.size
      val edgesToDualNode: Map[List[Edge], DNode] =
        tiling.orientedPolygons.zipWithIndex.map(
          (polygonPath, ordinal) =>
            polygonPath.toPolygonEdges -> Node(ordinal + perimeterSize + 1)
        ).toMap
      val polygonEdges: List[List[Edge]] =
        edgesToDualNode.keys.toList
      val internalDualEdges: List[DEdge] =
        tiling.nonPerimeterEdges.map(edge =>
          Edge(polygonEdges.filter(_.contains(edge)).map(edgesToDualNode(_)))
        )
      val perimeterDualEdges: List[DEdge] =
        perimeterEdges.indices.map(index =>
          Edge(Node(index + 1), edgesToDualNode(polygonEdges.find(_.contains(perimeterEdges(index))).get))
        ).toList
      new TilingDual(perimeterDualEdges ++ internalDualEdges, perimeterDualEdges.map(_.lesserNode).toVector)
