package io.github.scala_tessella.tessella

import Geometry.{Box, LineSegment, Point, Radian}
import Geometry.Radian.TAU_2
import Topology.{Edge, Node, NodeOrdering}
import utility.Utils.{mapValues2, toCouple}
import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO, startAt}

import scala.annotation.tailrec
import scala.collection.mutable

/** Methods to help the spatial representation of a tiling */
object TilingCoordinates:

  /** Associations of node and spatial 2D coordinates */
  type Coords = Map[Node, Point]

//  /** Spatial coordinates for the first two nodes of a [[Tiling]] */
//  private val startingCoords: Coords =
//    Map(Node(1) -> Point(), Node(2) -> Point(1, 0))

  extension (tiling: Tiling)

    /** Spatial coordinates of a [[Tiling]]
     *
     * @return the coordinates with the lowest node at origin and its lowest adjacent at 1,0
     */
    def coordinates: Coords =
      tiling.graphNodes.minOption(NodeOrdering) match
        case Some(first) =>
          coordinatesFinder(
            Edge(first, tiling.graphEdges.adjacentTo(first).min(NodeOrdering)),
            LineSegment(Point(), Point(1, 0))
          )
        case None => Map.empty

    /** Spatial coordinates of a [[Tiling]] from a localized node
     *
     * @param node
     * @param point
     * @return the coordinates with the given node at the given point and its lowest adjacent at +1 on the x-axis
     */
    def coordinatesFromStartingNode(node: Node, point: Point): Coords =
      if tiling.graphNodes.contains(node) then
        val otherNode: Node =
          tiling.graphEdges.adjacentTo(node).min(NodeOrdering)
        val deltaX: Double =
          if otherNode > node then 1 else -1
        coordinatesFinder(
          Edge(node, otherNode),
          LineSegment(point, point.plus(Point(deltaX, 0)))
        )
      else coordinates

    /** Spatial coordinates of a [[Tiling]] from a localized edge
     *
     * @param edge
     * @param lineSegment
     * @return the coordinates with the given edge at the given points
     */
    def coordinatesFromStartingEdge(edge: Edge, lineSegment: LineSegment): Coords =
      if tiling.graphEdges.contains(edge) && lineSegment.hasUnitLength() then
        coordinatesFinder(edge, lineSegment)
      else coordinates

    def coordinatesTriangulation(edge: Edge, lineSegment: LineSegment, node: Node, point: Point): Coords =
      if tiling.graphEdges.contains(edge) && lineSegment.hasUnitLength() && tiling.graphNodes.contains(node) then
        coordinatesFinder(edge, lineSegment, Some((node, point)))
      else coordinates

    private def coordinatesFinder(edge: Edge, lineSegment: LineSegment, maybeTriangle: Option[(Node, Point)] = None): Coords =
      if tiling.graphEdges.isEmpty then
        Map.empty
      else
        // Preprocessing: Create a map from Node to Polygons it belongs to
        val nodeToPolygonsMap: Map[Node, List[tiling.PolygonPath]] =
          tiling.orientedPolygons.foldLeft(Map.empty[Node, List[tiling.PolygonPath]].withDefaultValue(Nil)) { (acc, polygon) =>
            polygon.toPolygonPathNodes.foldLeft(acc) { (mapAcc, node) =>
              mapAcc + (node -> (polygon :: mapAcc(node)))
            }
          }

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

        maybeTriangle match
          case Some((third, point)) if point.almostEquals(coords(third))                => coords.toMap
          case Some((third, point)) if point.almostEquals(coords(third).flipVertically) => coords.toMap.flipVertically
          case _ =>
            val third: Node =
              tiling.graphEdges.adjacentTo(edge.greaterNode).filterNot(_ == edge.lesserNode).min(NodeOrdering)
            coords.get(third) match
              case Some(point) if point.y < 0 => coords.toMap.flipVertically
              case _                          => coords.toMap

  extension (coords: Coords)

    def flipVertically: Coords =
      coords.mapValues2(_.flipVertically)

    def flipVertically(node: Node, reference: Point): Option[Coords] =
      coords.get(node) match
        case Some(point) if reference.almostEquals(point)                => Some(coords)
        case Some(point) if reference.almostEquals(point.flipVertically) => Some(flipVertically)
        case _                                                           => None

    def transform(edge: Edge, lineSegment: LineSegment): Option[Coords] =
      val nodes: Vector[Node] = coords.keys.toVector
      if nodes.contains(edge.lesserNode)
        && nodes.contains(edge.greaterNode)
        && LineSegment(coords(edge.lesserNode), coords(edge.greaterNode)).hasAlmostEqualLength(lineSegment)
      then
        val currentPoint1 = coords(edge.lesserNode)
        val currentPoint2 = coords(edge.greaterNode)
        val currentLineSegment = LineSegment(currentPoint1, currentPoint2)

        // Calculate the transformation needed
        val currentAngle: Radian = currentLineSegment.horizontalAngle
        val targetAngle: Radian = lineSegment.horizontalAngle
        val rotationAngle: Radian = targetAngle - currentAngle

        // Transform all coordinates
        val transformedCoords =
          coords.map { (node, point) =>
            // First translate to origin (relative to current point1)
            val translatedPoint: Point = point.minus(currentPoint1)
            // Then rotate around origin
            val rotatedPoint: Point = translatedPoint.rotate(rotationAngle)
            // Finally translate to target position
            val finalPoint: Point = rotatedPoint.plus(lineSegment.point1)
            node -> finalPoint
          }

        Some(transformedCoords)
      else
        None

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
