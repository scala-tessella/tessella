package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU_2
import Topology.{Edge, Node, NodeOrdering}
import utility.Utils.toCouple
import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO, startAt}

import scala.collection.mutable

/** Methods to help the spatial representation of a tiling */
object TilingCoordinates:

  /** Associations of node and spatial 2D coordinates */
  type Coords = Map[Node, Point]

  extension (tiling: Tiling)

    /** Spatial coordinates of a [[Tiling]]
     *
     * @return the coordinates with the lowest node at origin and its lowest adjacent at 1,0
     */
    def coordinates: Coords =
      tiling.graphNodes.minOption(NodeOrdering) match
        case Some(first) =>
          buildCoordinates(
            Edge(first, tiling.graphEdges.adjacentTo(first).min(NodeOrdering)),
            LineSegment(Point(), Point(1, 0))
          )
        case None => Map.empty

    /** Spatial coordinates of a [[Tiling]] from a localized node
     *
     * @param node starting node
     * @param point coordinates of the starting node
     * @return the coordinates with the given node at the given point and its lowest adjacent at +1 on the x-axis
     */
    def coordinatesFromStartingNode(node: Node, point: Point): Coords =
      if tiling.graphNodes.contains(node) then
        val otherNode: Node =
          tiling.graphEdges.adjacentTo(node).min(NodeOrdering)
        val deltaX: Double =
          if otherNode > node then 1 else -1
        buildCoordinates(
          Edge(node, otherNode),
          LineSegment(point, point.plus(Point(deltaX, 0)))
        )
      else coordinates

    /** Spatial coordinates of a [[Tiling]] from a localized edge
     *
     * @param edge starting edge
     * @param lineSegment coordinates of the starting edge
     * @return the coordinates with the given edge at the given points
     */
    def coordinatesFromStartingEdge(edge: Edge, lineSegment: LineSegment): Coords =
      if tiling.graphEdges.contains(edge) && lineSegment.hasUnitLength() then
        buildCoordinates(edge, lineSegment)
      else coordinates

    /** Creates a map from Node to Polygons it belongs to */
    private def buildNodeToPolygonsMap: Map[Node, List[tiling.PolygonPath]] =
      tiling.orientedPolygons.foldLeft(Map.empty[Node, List[tiling.PolygonPath]].withDefaultValue(Nil)) { (acc, polygon) =>
        polygon.toPolygonPathNodes.foldLeft(acc) { (mapAcc, node) =>
          mapAcc + (node -> (polygon :: mapAcc(node)))
        }
      }

    private def buildCoordinates(edge: Edge, lineSegment: LineSegment): Coords =
      if tiling.graphEdges.isEmpty then
        Map.empty
      else
        // Preprocessing
        val nodeToPolygonsMap: Map[Node, List[tiling.PolygonPath]] =
          buildNodeToPolygonsMap

        val startingCoords: List[(Node, Point)] =
          List(
            (edge.lesserNode, lineSegment.point1),
            (edge.greaterNode, lineSegment.point2)
          )
        val coords: mutable.Map[Node, Point] = mutable.Map.from(startingCoords)
        val nodesToExplore: mutable.Queue[Node] = mutable.Queue.from(coords.keys)
        val processedPolygons: mutable.Set[tiling.PolygonPath] = mutable.Set.empty

        while nodesToExplore.nonEmpty do
          val currentNode = nodesToExplore.dequeue()

          nodeToPolygonsMap(currentNode).foreach { polygon =>
            if !processedPolygons.contains(polygon) then
              val polygonNodes = polygon.toPolygonPathNodes
              // Find an edge in this polygon where both nodes have known coordinates
              polygonNodes.slidingO(2).toVector.indexWhere(edgeNodes => edgeNodes.forall(coords.contains)) match
                case -1 => // No known edge found yet for this polygon with the current currentNode
                case knownEdgeIndex =>
                  // A known edge is found, calculate coordinates for other nodes in this polygon
                  val pairs: Vector[Vector[Node]] =
                    polygonNodes.slidingO(2).toVector

                  val startingAngle: Radian =
                    coords(pairs(knownEdgeIndex)(0)).angleTo(coords(pairs(knownEdgeIndex)(1)))
                  val alpha: Radian =
                    polygon.toPolygon.alpha

                  var currentAngle = startingAngle // This is the angle of the known edge
                  // Iterate from the known edge to calculate other points
                  pairs.startAt(knownEdgeIndex - 1).drop(2).map(_.toCouple).foreach {
                    case (previousNode, newNode) =>
                      // The angle must be updated for each segment traversal along the polygon's perimeter.
                      // This new currentAngle is the angle of the segment (previousNode, newNode).
                      currentAngle = currentAngle + TAU_2 - alpha
                      if coords.contains(previousNode) && !coords.contains(newNode) then
                        val newPoint = coords(previousNode).plusPolarUnit(currentAngle)
                        coords += (newNode -> newPoint)
                        if !nodesToExplore.contains(newNode) then // Avoid re-adding
                          nodesToExplore.enqueue(newNode)
                  }
                  // Check if all nodes of the polygon are now in coords, if so, mark as processed
                  if polygonNodes.forall(coords.contains) then
                    processedPolygons.add(polygon)
          }

        val third: Node =
          tiling.graphEdges.adjacentTo(edge.greaterNode).filterNot(_ == edge.lesserNode).min(NodeOrdering)
        coords.get(third) match
          case Some(point) if point.y < 0 => coords.toMap.flipVertically
          case _                          => coords.toMap

  extension (coords: Coords)

    def flipVertically: Coords =
      coords.view.mapValues(_.flipVertically).toMap

    /**
     * Tries to find a transformation (including reflection) that maps each of the three
     * originalNodes to each of the given targetPoints.
     * If the triangles do not have the same shape (are not congruent up to reflection), returns None.
     * Otherwise, returns a transformed Coords where all node coordinates are transformed accordingly.
     *
     * @param originalNodes Triple of nodes (A, B, C) referencing original triangle in coords
     * @param targetPoints  Triple of new points (A', B', C') desired for the mapped triangle
     * @return Option[Coords] with all points transformed, or None if impossible
     */
    def affine(originalNodes: (Node, Node, Node), targetPoints: (Point, Point, Point)): Option[Coords] =

      val (aN, bN, cN) = originalNodes
      val (aP, bP, cP) = (coords.get(aN), coords.get(bN), coords.get(cN))
      val (aQ, bQ, cQ) = targetPoints

      (aP, bP, cP) match
        case (Some(a), Some(b), Some(c)) =>
          val originalTriangle: Vector[Point] = Vector(a, b, c)
          val targetTriangle: Vector[Point] = Vector(aQ, bQ, cQ)

          // Check if triangles are congruent
          if !originalTriangle.isCongruentTo(targetTriangle) then None
          else
            // Vectors from first point to others
            val v1 = b.minus(a) // AB
            val v2 = c.minus(a) // AC
            val u1 = bQ.minus(aQ) // A'B'
            val u2 = cQ.minus(aQ) // A'C'

            // Find the linear transformation matrix
            Matrix2x2.findTransform(v1, v2, u1, u2) match
              case None => None // Degenerate triangle
              case Some(matrix) =>
                // Compose full affine transform: p' = M * (p - a) + aQ
                def transform(p: Point): Point =
                  matrix.transform(p.minus(a)).plus(aQ)

                // Apply transform to all nodes
                Some(coords.view.mapValues(transform).toMap)
        case _ => None

  extension (nodes: Vector[Node])

    def pointsFrom(angles: Map[Node, Radian]): Vector[Point] =
      nodes.scanLeft((Point(1, 0), TAU_2: Radian))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + TAU_2)
      }).map((point, _) => point).tail

  extension (edge: Edge)

    /** Creates a `LineSegment` of the given coordinates */
    def toSegment(coords: Coords): LineSegment =
      LineSegment(coords(edge.lesserNode), coords(edge.greaterNode))

  extension (edges: List[Edge])

    /** Creates a sequence of `LineSegment` of the given coordinates */
    def toSegments(coords: Coords): List[LineSegment] =
      edges.map(_.toSegment(coords))

    /** Creates a bounding `Box` containing all edges with the given coordinates
     *
     * @param coords      map with the points of each node
     * @param enlargement extra space at each side
     */
    def toBox(coords: Coords, enlargement: Double = 0.0): Box =
      val points: List[Point] =
        edges.nodes.map(coords)
      val xs: List[Double] =
        points.map(_.x)
      val ys: List[Double] =
        points.map(_.y)
      Box(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)
