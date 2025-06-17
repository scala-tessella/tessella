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

    /** Spatial coordinates for the first two nodes of a [[Tiling]] */
    private def getStartingCoords: Coords =
      tiling.graphNodes.minOption(NodeOrdering) match
        case Some(first) =>
          Map(
            first -> Point(),
            tiling.graphEdges.adjacentTo(first).min(NodeOrdering) -> Point(1, 0)
          )
        case None => Map.empty

  /** Spatial coordinates of a [[Tiling]] */
    def coordinatesOld: Coords =

      @tailrec
      def loop(coords: Coords, polygons: List[tiling.PolygonPath]): Coords =
        polygons.find(_.toPolygonPathNodes.slidingO(2).exists(_.forall(node => coords.contains(node)))) match
          case None => coords
          case Some(polygon) =>
            val pairs: Vector[Vector[Node]] =
              polygon.toPolygonPathNodes.slidingO(2).toVector
            val index: Index =
              pairs.indexWhere(_.forall(node => coords.contains(node)))
            val startingAngle: Radian =
              coords(pairs(index)(0)).angleTo(coords(pairs(index)(1)))
            val alpha: Radian =
              polygon.toPolygon.alpha
            val newCoords: Coords =
              pairs.startAt(index - 1).drop(2).map(_.toCouple).foldLeft((coords, startingAngle))({
                case ((cumulativeCoordinates, angle), (previous, node)) =>
                  val newAngle: Radian =
                    angle + TAU_2 - alpha
                  val newCumulativeCoordinates: Coords =
                    if cumulativeCoordinates.contains(node) then cumulativeCoordinates
                    else cumulativeCoordinates + (node -> cumulativeCoordinates(previous).plusPolarUnit(newAngle))
                  (newCumulativeCoordinates, newAngle)
              })._1
            val newPolygons: List[tiling.PolygonPath] =
              polygons.diff(List(polygon)).filter(_.toPolygonPathNodes.exists(node => !newCoords.contains(node)))
            loop(newCoords, newPolygons)

      if tiling.graphEdges.isEmpty then
        Map()
      else
        loop(getStartingCoords, tiling.orientedPolygons).flipVertically

  /** Spatial coordinates of a [[Tiling]] */
    def coordinates: Coords =
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

        val coords: mutable.Map[Node, Point] = mutable.Map.from(getStartingCoords)
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
        coords.toMap.flipVertically

  extension (nodes: Vector[Node])

    def pointsFrom(angles: Map[Node, Radian]): Vector[Point] =
      nodes.scanLeft((Point(1, 0), TAU_2: Radian))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + TAU_2)
      }).map((point, _) => point).tail

  extension (coords: Coords)

    /** New coordinates guaranteeing that the third node of a [[Tiling]] has always a positive y value */
    def flipVertically: Coords =
      val sortedNodes = coords.keys.toVector.sorted(NodeOrdering)
      coords.get(sortedNodes(2)) match
        case Some(point) if point.y < 0 => coords.mapValues2(_.flipVertically)
        case _                          => coords

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
