package io.github.scala_tessella.tessella

import conversion.DOT.toDOT
import utility.Utils.{filterNotUnique, filterUnique}
import Geometry.{Box, LESSER_ACCURACY, LineSegment, Point, Radian}
import Geometry.Radian.{TAU, TAU_2}
import IncrementalTiling.Strictness
import RegularPolygon.Polygon
import TilingCoordinates.{Coords, pointsFrom, toBox}
import Topology.{Edge, Node, NodeOrdering}
import io.github.scala_tessella.ring_seq.RingSeq.*

// The IncrementalTiling class is now a "dumb" data holder. Its primary role is to hold
// consistent, pre-computed data. It's immutable.
case class IncrementalTiling private(
                                      orientedPolygons: List[Vector[Node]],
                                      perimeter: Vector[Node],
                                      coordinates: Coords
                                    ):
  // Edges are derived from the polygons, ensuring consistency.
  lazy val edges: List[Edge] = orientedPolygons.flatMap(_.toEdgesO).distinct

  // Methods on IncrementalTiling now simply return the pre-computed properties.
  // No expensive calculations happen here.
  def getPerimeter: Vector[Node] = perimeter
  def getPolygons: List[Vector[Node]] = orientedPolygons
  def getCoords: Coords = coordinates

  // You can still have other derived values, but the expensive ones are now vals.
  val perimeterLength: Int = perimeter.size

  def isEmpty: Boolean =
    orientedPolygons.isEmpty

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

    // 1. Determine orientation by perimeter traversal

    val (startNode, nextNode) =
      perimeter.slidingO(2).find(p => (p.head == n1 && p.last == n2) || (p.head == n2 && p.last == n1)) match
        case Some(pair) => (pair.head, pair.last) // Build on the reversed edge to go "outwards".
        case None => throw new AssertionError(s"Edge $perimeterEdge not found on perimeter.")

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
        newPoint.almostEquals(coordinates(perimeterNode), LESSER_ACCURACY) && !perimeterEdge.nodes.contains(perimeterNode)
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

  private def updatePerimeter(poly: Vector[Node]): Vector[Node] =
    // 1. Find nodes shared between the perimeter and the given polygon.
    val sharedNodes = perimeter.intersect(poly)

    // 2. From the shared nodes, find the segment of the perimeter that will be replaced.
    val sharedNodesCount = sharedNodes.size
    val orderedSharedNodes = perimeter.slidingO(sharedNodesCount)
      .find(p => p.toSet == sharedNodes.toSet)
      .get
    val startNode = orderedSharedNodes.head
    val endNode = orderedSharedNodes.last

    // 3. The new perimeter segment is formed by the edges of the given polygon
    //    that were not on the old perimeter.
    val edgesNotOnPerimeter = poly.toEdgesO.toList.diff(perimeter.toEdgesO.toList)
    val newPathSegment = edgesNotOnPerimeter.shortestPath(startNode, endNode)

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
          val nearbyPerimeterEdges =
            perimeterEdgesToCheck.filter { edge =>
              val p1 = allCoords(edge.lesserNode)
              val p2 = allCoords(edge.greaterNode)
              LineSegment(p1, p2).hasEndpointIn(enlargedNewPolygonBox)
            }

          val crossingExists =
            additionalEdges.exists { additionalEdge =>
              val p1 = allCoords(additionalEdge.lesserNode)
              val p2 = allCoords(additionalEdge.greaterNode)
              nearbyPerimeterEdges.exists { perimeterEdge =>
                val p3 = allCoords(perimeterEdge.lesserNode)
                val p4 = allCoords(perimeterEdge.greaterNode)
                LineSegment(p1, p2).intersectsStrict(LineSegment(p3, p4))
              }
            }

          crossingExists
        } then
          Left("Invalid addition: new polygon's edges cross perimeter.")
        else
          val newPolygons = orientedPolygons :+ mergedPolygon
          val newCoords = coordinates ++ finalAdditionalCoords
          val newPerimeter = updatePerimeter(mergedPolygon)

          Right(IncrementalTiling(newPolygons, newPerimeter, newCoords))
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

    // Nodes of the removed polygon that were NOT on the perimeter.
    val subtractableNodes = touchEdges.threadNodes
    val newCoords = coordinates -- subtractableNodes

    val sortedPathNodes = polygonPath.sorted(NodeOrdering)
    val newPolygons = orientedPolygons.filterNot(_.sorted(NodeOrdering) == sortedPathNodes)

    // The new perimeter is formed by the old perimeter, with the `touchEdges` segment
    // replaced by the `subtractableEdges` segment. The existing updatePerimeter method correctly handles this.
    val newPerimeter = updatePerimeter(polygonPath)
    Right(IncrementalTiling(newPolygons, newPerimeter, newCoords))

  /** Finds the 2D box */
  def toBox: Box =
    edges.toBox(coordinates)

  def toDOT: String =
    Graph(edges).toDOT()

// The companion object becomes the "smart" factory and manager.
// It contains all the logic for creating and growing tilings,
// ensuring that any instance of TilingAlt is always valid and consistent.
object IncrementalTiling:

  /**
   * Creates an empty tiling.
   */
  def empty: IncrementalTiling =
    IncrementalTiling(Nil, Vector.empty, Map.empty)

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
    val initialPolygons: List[Vector[Node]] = List(initialPerimeter)
    val angle: Radian = Polygon(validatedSides).alpha
    val angles: Map[Node, Radian] = initialPerimeter.map(_ -> angle).toMap
    val points: Vector[Point] = initialPerimeter.pointsFrom(angles)
    val initialCoords: Coords = initialPerimeter.zip(points).toMap

    // 2. Call the private constructor with the fully consistent data.
    IncrementalTiling(initialPolygons, initialPerimeter, initialCoords)
  //    fromTiling(Tiling.fromPolygon(sides))

  def fromTiling(tiling: Tiling): IncrementalTiling =
    IncrementalTiling(
      tiling.orientedPolygons.map(_.toPolygonPathNodes),
      tiling.perimeter.toRingNodes,
      tiling.coords
    )

  /**
   * Creates an [[IncrementalTiling]] only after passing validation checks.
   *
   * @param orientedPolygons list of polygons as ordered nodes
   * @param perimeter        ordered nodes of the perimeter
   * @param coordinates      map of nodes to points
   * @return an `Either` with a `String` error message or the created `IncrementalTiling`
   */
  def maybe(
             orientedPolygons: List[Vector[Node]],
             perimeter: Vector[Node],
             coordinates: Coords): Either[String, IncrementalTiling] =
    if orientedPolygons.isEmpty then
      if perimeter.isEmpty && coordinates.isEmpty then Right(empty)
      else Left("For empty polygons, perimeter and coordinates must also be empty.")
    else
      val tiling = IncrementalTiling(orientedPolygons, perimeter, coordinates)
      for
        _ <- validatePerimeter(tiling)
        _ <- validateConnectivity(tiling)
        _ <- validateCoordinates(tiling)
      yield tiling

  private def validatePerimeter(tiling: IncrementalTiling): Either[String, Unit] =
    val calculatedPerimeterEdges = filterUnique(tiling.orientedPolygons.flatMap(_.toEdgesO)).toSet
    val givenPerimeterEdges = tiling.perimeter.toEdgesO.toSet
    Either.cond(
      calculatedPerimeterEdges == givenPerimeterEdges,
      (),
      s"Perimeter edges are inconsistent. Given: $givenPerimeterEdges, Calculated: $calculatedPerimeterEdges"
    )

  private def validateConnectivity(tiling: IncrementalTiling): Either[String, Unit] =
    Either.cond(Graph(tiling.edges).isConnected, (), "The tiling graph is not connected.")


  private def validateCoordinates(tiling: IncrementalTiling): Either[String, Unit] =
    import scala.util.boundary, boundary.break

    boundary:
      val allNodes = tiling.edges.flatMap(e => List(e.lesserNode, e.greaterNode)).toSet
      val coordNodes = tiling.coordinates.keySet
      if allNodes != coordNodes then
        break(Left(s"Mismatch between graph nodes and coordinate nodes. Missing: ${allNodes -- coordNodes}, Extra: ${coordNodes -- allNodes}"))

      for edge <- tiling.edges do
        val p1 = tiling.coordinates(edge.lesserNode)
        val p2 = tiling.coordinates(edge.greaterNode)
        if Math.abs(p1.distanceTo(p2) - 1.0) > Geometry.LESSER_ACCURACY then
          break(Left(s"Edge ${edge.stringify} has length not equal to 1."))

      // this is ok only if Strictness is STRICT
//      val points = tiling.coordinates.values.toList
//      if points.combinations(2).exists(p => p.head.almostEquals(p.last)) then
//        break(Left("Found distinct nodes with almost identical coordinates."))

      for polygonPath <- tiling.orientedPolygons do
        val polygon = Polygon(polygonPath.size)
        val expectedAngle = polygon.alpha
        for i <- polygonPath.indices do
          val pPrev = tiling.coordinates(polygonPath.applyO(i - 1))
          val pCurr = tiling.coordinates(polygonPath.applyO(i))
          val pNext = tiling.coordinates(polygonPath.applyO(i + 1))

          val angle = pCurr.angleTo(pNext) - pCurr.angleTo(pPrev)
          val normalizedAngle = if angle.toDouble < 0 then angle + TAU else angle

          if Math.abs(normalizedAngle.toDouble - expectedAngle.toDouble) > Geometry.LESSER_ACCURACY then
            break(Left(s"Invalid interior angle for polygon ${polygonPath.mkString(",")} at node ${polygonPath.applyO(i)}."))

      Right(())

  enum Strictness:

    case STRICT, TOUCHING, CROSSING