package io.github.scala_tessella.tessella

import Geometry.{Box, Point, Radian}
import Geometry.Radian.TAU_2
import IncrementalTiling.Strictness
import RegularPolygon.Polygon
import TilingCoordinates.{Coords, pointsFrom, toBox}
import Topology.{Edge, Node, NodeOrdering}

import io.github.scala_tessella.ring_seq.RingSeq.*

// The IncrementalTiling class is now a "dumb" data holder. Its primary role is to hold
// consistent, pre-computed data. It's immutable.
case class IncrementalTiling private(
                               edges: List[Edge],
                               orientedPolygons: List[Vector[Node]],
                               perimeter: Vector[Node],
                               coordinates: Coords
                             ):
  // Methods on IncrementalTiling now simply return the pre-computed properties.
  // No expensive calculations happen here.
  def getPerimeter: Vector[Node] = perimeter
  def getPolygons: List[Vector[Node]] = orientedPolygons
  def getCoords: Coords = coordinates

  // You can still have other derived values, but the expensive ones are now vals.
  val perimeterLength: Int = perimeter.size

  def isEmpty: Boolean =
    edges.isEmpty

  def hasOnPerimeter(edge: Edge): Boolean =
    perimeter.toEdgesO.contains(edge)

  def maxNode: Node =
    coordinates.keys.maxBy(_.toInt)

  def perimeterOrientedPolygons: List[Vector[Node]] =
    val perimeterEdgeSet = perimeter.toEdgesO.toSet
    orientedPolygons.filter(_.toEdgesO.exists(perimeterEdgeSet.contains))

  def perimeterPolygonsContain(polygonPath: Vector[Node]): Boolean =
    val pathRotationsAndReflections = polygonPath.rotationsAndReflections.toSet
    perimeterOrientedPolygons.exists(pathRotationsAndReflections.contains)

  def polygonsContain(polygonPath: Vector[Node]): Boolean =
    val pathRotationsAndReflections = polygonPath.rotationsAndReflections.toSet
    orientedPolygons.exists(pathRotationsAndReflections.contains)

  def perimeterTouchpoints(polygonPath: Vector[Node]): (List[Edge], List[Node]) =
    val touchNodes = perimeter.intersect(polygonPath).toList
    val perimeterEdgeSet = perimeter.toEdgesO.toSet
    val touchEdges = polygonPath.toEdgesO.filter(perimeterEdgeSet.contains).toList
    (touchEdges, touchNodes)

  /**
   * Calculates the coordinates for the new vertices of a regular polygon attached to an existing tiling's perimeter.
   *
   * @return A tuple containing the ordered path of the new polygon (nodes) and a `Coords` map
   *         for only the newly created vertices.
   */
  def calculateNewPolygonCoords(polygon: Polygon, perimeterEdge: Edge): (Vector[Node], Coords) =
    val (n1, n2) = perimeterEdge.pair

    // --- ALTERNATIVE 1: Determine orientation by perimeter traversal ---

    val (startNode, nextNode) =
      perimeter.slidingO(2).find(p => (p.head == n1 && p.last == n2) || (p.head == n2 && p.last == n1)) match
        case Some(pair) => (pair.head, pair.last) // Build on the reversed edge to go "outwards".
        case None => throw new AssertionError(s"Edge $perimeterEdge not found on perimeter.")


    // --- ALTERNATIVE 2: Determine orientation by checking adjacent polygon's spatial location (commented out) ---
    /* val (startNode, nextNode) = {
      // Find the polygon that already contains this edge. Because it's a perimeter edge, there is only one.
      val adjacentPolygon = existingTiling.orientedPolygons
        .find(p => p.slidingO(2).exists(pair => Edge(pair) == perimeterEdge))
        .getOrElse(throw new IllegalStateException(s"No adjacent polygon found for perimeter edge $perimeterEdge"))
  
      // Find a third node in that polygon to determine its orientation relative to the edge.
      val n1_index = adjacentPolygon.indexOf(n1)
      val thirdNode = if (adjacentPolygon.applyO(n1_index + 1) == n2) adjacentPolygon.applyO(n1_index - 1) else adjacentPolygon.applyO(n1_index + 1)
  
      val coords = existingTiling.coordinates
      val (p1, p2, p3) = (coords(n1), coords(n2), coords(thirdNode))
  
      // If the third point is to the "left" of the directed edge p1->p2, the new polygon should be constructed
      // counter-clockwise from p2->p1 to build "outward". Otherwise, from p1->p2.
      if (Point.ccw(p1, p2, p3) > 0) (n2, n1) else (n1, n2)
    }
    */
    // 2. Generate the required number of new nodes for the polygon.
    val sides = polygon.toSides
    val newNodesCount = sides - 2
    val maxExistingNode = maxNode.toInt
    val newNodes = Vector.tabulate(newNodesCount)(i => Node(maxExistingNode + 1 + i))

    // 3. Iteratively calculate the coordinates of the new nodes.
    val alpha = polygon.alpha
    val coords = coordinates
    val turnAngle = TAU_2 - alpha // The angle to turn at each vertex for a CCW traversal.

    var currentAngle = coords(startNode).angleTo(coords(nextNode))
    var currentPoint = coords(nextNode)

    val newCoords = collection.mutable.Map.empty[Node, Point]

    for (newNode <- newNodes) {
      currentAngle += turnAngle
      val newPoint = currentPoint.plusPolarUnit(currentAngle)
      newCoords(newNode) = newPoint
      currentPoint = newPoint
    }

    val newPolygon = (Vector(startNode, nextNode) ++ newNodes).reverse
    (newPolygon, newCoords.toMap)

  private def mergeCoincidentNodes(
                                    newPolygon: Vector[Node],
                                    newCoords: Coords,
                                    perimeterEdge: Edge,
                                    strictness: Strictness
                                  ): Either[String, (Vector[Node], Coords)] =
    // Find all new nodes that are coincident with any node on the perimeter.
    val allCoincidences = newCoords.toList.flatMap { case (newNode, newPoint) =>
      perimeter.find { perimeterNode =>
        newPoint.almostEquals(coordinates(perimeterNode)) && !perimeterEdge.nodes.contains(perimeterNode)
      }.map(perimeterNode => newNode -> perimeterNode)
    }.toMap

    if allCoincidences.isEmpty then
      return Right((newPolygon, newCoords))

    // From the perimeter edge, find the two nodes that define the attachment points, ordered by perimeter traversal.
    val (p1, p2) =
      if perimeter.applyO(perimeter.indexOf(perimeterEdge.lesserNode) + 1) == perimeterEdge.greaterNode then
        (perimeterEdge.lesserNode, perimeterEdge.greaterNode)
      else
        (perimeterEdge.greaterNode, perimeterEdge.lesserNode)

    val matchedPerimeterNodes = allCoincidences.values.toSet

    // Trace forwards or backwards from the given index to find a contiguous block of coincident nodes.
    def contiguousFromIndex(index: Index, isForward: Boolean = true): List[Node] =
      val increment: Int = if isForward then 1 else -1
      LazyList.from(increment, increment).map(i => perimeter.applyO(index + i))
        .takeWhile(matchedPerimeterNodes.contains)
        .toList

    val validPerimeterNodes =
      (contiguousFromIndex(perimeter.indexOf(p1), isForward = false)
        ++ contiguousFromIndex(perimeter.indexOf(p2))).toSet

    // Only perform substitutions for the valid, contiguous nodes.
    // A stricter future implementation might throw an error if (matchedPerimeterNodes -- validPerimeterNodes).nonEmpty
    if strictness == Strictness.STRICT && (matchedPerimeterNodes -- validPerimeterNodes).nonEmpty then
      val outsideNodes = matchedPerimeterNodes -- validPerimeterNodes
      return Left(s"Coincident nodes ${outsideNodes.mkString(", ")} outside of the share edges.")
    val substitutions = allCoincidences.filter { case (_, perimeterNode) =>
      validPerimeterNodes.contains(perimeterNode)
    }

    if substitutions.isEmpty then
      Right((newPolygon, newCoords))
    else
      val mergedPolygon = newPolygon.map(node => substitutions.getOrElse(node, node))
      val mergedCoords = newCoords -- substitutions.keys
      Right((mergedPolygon, mergedCoords))

  private def updatePerimeterOnAddition(poly: Vector[Node]): Vector[Node] =
    val sharedNodes = perimeter.intersect(poly)

    //    if (sharedNodes.size < 2) {
    //
    //      if (poly.toSet == perimeter.toSet) return Vector.empty // Polygon filled the only hole
    //      return perimeter
    //    }

    // Identify the segment of the perimeter to be replaced.
    // This is the path between the first and last nodes shared with the new polygon.
    val sharedNodesCount = sharedNodes.size
    val orderedSharedNodes = perimeter.slidingO(sharedNodesCount).find(p => p.diff(sharedNodes).isEmpty).get
    val startNode = orderedSharedNodes.head
    val endNode = orderedSharedNodes.last

    // Find the new path from the added polygon that connects the start and end nodes.
    val newPolyEdges = poly.toEdgesO.toList.diff(perimeter.toEdgesO.toList)
    val newPathSegment = newPolyEdges.shortestPath(startNode, endNode)

    val index = perimeter.indexOfSliceO(orderedSharedNodes)
    val partial = perimeter.startAt(index).drop(sharedNodesCount)

    partial ++ newPathSegment

  private def updatePerimeterOnRemoval(poly: Vector[Node]): Vector[Node] =
    // 1. Find nodes shared between the perimeter and the polygon being removed.
    val sharedNodes = perimeter.intersect(poly)

    // 2. From the shared nodes, find the segment of the perimeter that will be replaced.
    //    Validation should ensure these nodes are contiguous.
    val sharedNodesCount = sharedNodes.size
    val orderedSharedNodes = perimeter.slidingO(sharedNodesCount)
      .find(p => p.toSet == sharedNodes.toSet)
      .get
    val startNode = orderedSharedNodes.head
    val endNode = orderedSharedNodes.last

    // 3. The new perimeter segment is formed by the edges of the removed polygon
    //    that were *internal* to the tiling (i.e., not on the old perimeter).
    val internalPolyEdges = poly.toEdgesO.toList.diff(perimeter.toEdgesO.toList)
    val newPathSegment = internalPolyEdges.shortestPath(startNode, endNode)

    // 4. Identify the part of the original perimeter that will be kept.
    val index = perimeter.indexOfSliceO(orderedSharedNodes)
    val partial = perimeter.startAt(index).drop(sharedNodesCount)

    // 5. The new perimeter is the kept part plus the new segment.
    partial ++ newPathSegment

  /**
   * The incremental growth method. This is where the performance gain is.
   *
   * @param polygon        Information about the new polygon.
   * @param perimeterEdge  The perimeter edge the new polygon must be attached to.
   * @param strictness     Validation strictness
   * @return Either an error or the new, larger TilingAlt.
   */
  def addPolygon(
                  polygon: Polygon,
                  perimeterEdge: Edge,
                  strictness: Strictness = Strictness.STRICT
                ): Either[String, IncrementalTiling] =

    if !hasOnPerimeter(perimeterEdge) then
      return Left(s"Perimeter edge ${perimeterEdge.stringify} not found.")

    val (initialPolygon, additionalCoords) = calculateNewPolygonCoords(polygon, perimeterEdge)

    def segmentsIntersect(p1: Point, p2: Point, p3: Point, p4: Point): Boolean =
      Point.ccw(p1, p2, p3) * Point.ccw(p1, p2, p4) < 0 &&
        Point.ccw(p3, p4, p1) * Point.ccw(p3, p4, p2) < 0

    mergeCoincidentNodes(initialPolygon, additionalCoords, perimeterEdge, strictness)
      .flatMap((mergedPolygon, finalAdditionalCoords) =>
        val additionalEdges = mergedPolygon.toEdgesO.toList.diff(perimeter.toEdgesO.toList)

        if strictness != Strictness.CROSSING && {

          val perimeterEdgesToCheck = perimeter.toEdgesO.toList.diff(mergedPolygon.toEdgesO.toList)
          val allCoords = coordinates ++ finalAdditionalCoords

          // Create a bounding box for the new polygon's additional edges
          val newPolygonPoints = additionalEdges.nodes.map(allCoords)
          val newPolygonXs = newPolygonPoints.map(_.x)
          val newPolygonYs = newPolygonPoints.map(_.y)
          val newPolygonBox = Box(newPolygonXs.min, newPolygonXs.max, newPolygonYs.min, newPolygonYs.max)
          // Enlarge the box by 1 unit to create a "safety margin"
          val enlargedNewPolygonBox = newPolygonBox.enlarge(1.0)

          // Filter the perimeter edges to check to only those whose own bounding box intersects the enlarged box
          val nearbyPerimeterEdges = perimeterEdgesToCheck.filter { edge =>
            val p1 = allCoords(edge.lesserNode)
            val p2 = allCoords(edge.greaterNode)
            val edgeBox = Box(Math.min(p1.x, p2.x), Math.max(p1.x, p2.x), Math.min(p1.y, p2.y), Math.max(p1.y, p2.y))

            // Check for bounding box intersection
            enlargedNewPolygonBox.x1 >= edgeBox.x0 && enlargedNewPolygonBox.x0 <= edgeBox.x1 &&
              enlargedNewPolygonBox.y1 >= edgeBox.y0 && enlargedNewPolygonBox.y0 <= edgeBox.y1
          }

          val crossingExists =
            additionalEdges.exists { additionalEdge =>
              val p1 = allCoords(additionalEdge.lesserNode)
              val p2 = allCoords(additionalEdge.greaterNode)
              nearbyPerimeterEdges.exists { perimeterEdge =>
                val p3 = allCoords(perimeterEdge.lesserNode)
                val p4 = allCoords(perimeterEdge.greaterNode)
                segmentsIntersect(p1, p2, p3, p4)
              }
            }

          crossingExists
        } then
          Left("Invalid addition: new polygon's edges cross perimeter.")
        else
          val newEdges = edges ++ additionalEdges
          val newPolygons = orientedPolygons :+ mergedPolygon
          val newCoords = coordinates ++ finalAdditionalCoords
          val newPerimeter = updatePerimeterOnAddition(mergedPolygon)

          Right(IncrementalTiling(newEdges, newPolygons, newPerimeter, newCoords))
      )

  private def polygonDescription(polygonPath: Vector[Node]): String =
    s"Polygon with edges ${polygonPath.stringify}--"

  private def errorDescription(polygonPath: Vector[Node], touchEdges: List[Edge]): String =
    s"${polygonDescription(polygonPath)} shares edges ${touchEdges.map(_.stringify).mkString(", ")} with perimeter."

  def removePolygon(polygonPath: Vector[Node]): Either[String, IncrementalTiling] =
    // When removing the last polygon, the new perimeter is empty.
    if (polygonPath.toSet == perimeter.toSet) return Right(IncrementalTiling.empty)

    val (touchEdges, touchNodes) = perimeterTouchpoints(polygonPath)
    if touchEdges.isEmpty then
      return Left(s"${polygonDescription(polygonPath)} doesn't share any with perimeter.")
    val rogueTouchNodes =
      touchNodes.diff(touchEdges.nodes)
    if rogueTouchNodes.nonEmpty then
      return Left(s"Invalid shared separate nodes: ${rogueTouchNodes.mkString(", ")}. ${errorDescription(polygonPath, touchEdges)}")
    if !touchEdges.areContinuous then
      return Left(s"Non-continuos shared edges. ${errorDescription(polygonPath, touchEdges)}")
    if !perimeterPolygonsContain(polygonPath) then
      return Left(s"Polygon does not exist. ${errorDescription(polygonPath, touchEdges)}")

    // Edges of the removed polygon that were NOT on the perimeter.
    //    val subtractableEdges = polygonPath.toEdgesO.toList.diff(touchEdges)
    val newEdges = edges.diff(touchEdges)
    // Nodes of the removed polygon that were NOT on the perimeter.
    val subtractableNodes = touchEdges.threadNodes
    val newCoords = coordinates -- subtractableNodes

    val sortedPathNodes = polygonPath.sorted(NodeOrdering)
    val newPolygons = orientedPolygons.filterNot(_.sorted(NodeOrdering) == sortedPathNodes)

    // The new perimeter is formed by the old perimeter, with the `touchEdges` segment
    // replaced by the `subtractableEdges` segment. The existing updatePerimeter method correctly handles this.
    val newPerimeter = updatePerimeterOnRemoval(polygonPath)

    Right(
      IncrementalTiling(
        newEdges,
        newPolygons,
        newPerimeter,
        newCoords
      )
    )

  /** Finds the 2D box */
  def toBox: Box =
    edges.toBox(coordinates)

// The companion object becomes the "smart" factory and manager.
// It contains all the logic for creating and growing tilings,
// ensuring that any instance of TilingAlt is always valid and consistent.
object IncrementalTiling:

  /**
   * Creates an empty tiling.
   */
  def empty: IncrementalTiling =
    IncrementalTiling(Nil, Nil, Vector.empty, Map.empty)

  /** A tiling made of a single polygon.
   *
   * @param sides polygon or its number of sides
   * @throws IllegalArgumentException if sides <= 2
   */
  def fromPolygon(sides: Polygon | Int): IncrementalTiling =
    val validatedSides: Int =
      (sides: @unchecked) match
        case i: Int => Polygon(i).toSides
    // 1. Calculate properties from scratch, ONLY for this initial case.
    val initialPerimeter: Vector[Node] = Vector.range(1, validatedSides + 1).map(Node(_))
    val initialEdges: List[Edge] = initialPerimeter.toEdgesO.toList
    val initialPolygons: List[Vector[Node]] = List(initialPerimeter)
    val angle: Radian = Polygon(validatedSides).alpha
    val angles: Map[Node, Radian] = initialPerimeter.map(_ -> angle).toMap
    val points: Vector[Point] = initialPerimeter.pointsFrom(angles)
    val initialCoords: Coords = initialPerimeter.zip(points).toMap

    // 2. Call the private constructor with the fully consistent data.
    IncrementalTiling(initialEdges, initialPolygons, initialPerimeter, initialCoords)
  //    fromTiling(Tiling.fromPolygon(sides))

  def fromTiling(tiling: Tiling): IncrementalTiling =
    IncrementalTiling(
      tiling.graphEdges,
      tiling.orientedPolygons.map(_.toPolygonPathNodes),
      tiling.perimeter.toRingNodes,
      tiling.coords
    )

  enum Strictness:

    case STRICT, TOUCHING, CROSSING