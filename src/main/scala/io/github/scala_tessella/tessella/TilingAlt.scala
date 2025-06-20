package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import TilingCoordinates.{Coords, pointsFrom}
import Topology.{Edge, Node}
import io.github.scala_tessella.tessella.Geometry.Radian.TAU_2
import io.github.scala_tessella.tessella.Geometry.{Point, Radian}
import io.github.scala_tessella.ring_seq.RingSeq.{applyO, slidingO}

// The TilingAlt class is now a "dumb" data holder. Its primary role is to hold
// consistent, pre-computed data. It's immutable.
case class TilingAlt private (
                               edges: List[Edge],
                               orientedPolygons: List[Vector[Node]],
                               perimeter: Vector[Node],
                               coordinates: Coords
                             ):
  // Methods on TilingAlt now simply return the pre-computed properties.
  // No expensive calculations happen here.
  def getPerimeter: Vector[Node] = perimeter
  def getPolygons: List[Vector[Node]] = orientedPolygons
  def getCoords: Coords = coordinates

  // You can still have other derived values, but the expensive ones are now vals.
  val perimeterLength: Int = perimeter.size

  def hasOnPerimeter(edge: Edge): Boolean =
    perimeter.toEdgesO.contains(edge)

  def maxNode: Node =
    coordinates.keys.maxBy(_.toInt)

// The companion object becomes the "smart" factory and manager.
// It contains all the logic for creating and growing tilings,
// ensuring that any instance of TilingAlt is always valid and consistent.
object TilingAlt:

  /**
   * Creates an empty tiling. This is the starting point.
   */
  def empty: TilingAlt =
    TilingAlt(Nil, Nil, Vector.empty, Map.empty)

  /** A tiling made of the edges of the given polygon
   *
   * @param sides polygon or its number of sides
   * @throws IllegalArgumentException if sides <= 2
   */
  def fromPolygon(sides: Polygon | Int): TilingAlt =
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
    TilingAlt(initialEdges, initialPolygons, initialPerimeter, initialCoords)
//    fromTiling(Tiling.fromPolygon(sides))

  def fromTiling(tiling: Tiling) =
    TilingAlt(
      tiling.graphEdges, 
      tiling.orientedPolygons.map(_.toPolygonPathNodes), 
      tiling.perimeter.toRingNodes, 
      tiling.coords
    )

  /**
   * Calculates the coordinates for the new vertices of a regular polygon attached to an existing tiling's perimeter.
   *
   * @return A tuple containing the ordered path of the new polygon (nodes) and a `Coords` map
   *         for only the newly created vertices.
   */
  private def calculateNewPolygonCoords(
                                         existingTiling: TilingAlt,
                                         polygon: Polygon,
                                         perimeterEdge: Edge
                                       ): (Vector[Node], Coords) = {
    val (n1, n2) = (perimeterEdge.lesserNode, perimeterEdge.greaterNode)

    // --- ALTERNATIVE 1: Determine orientation by perimeter traversal (commented out) ---
    /*
    val (startNode, nextNode) =
      existingTiling.perimeter.slidingO(2).find(p => (p.head == n1 && p.last == n2) || (p.head == n2 && p.last == n1)) match {
        case Some(pair) => (pair.last, pair.head) // Build on the reversed edge to go "outwards".
        case None => throw new AssertionError(s"Edge $perimeterEdge not found on perimeter.")
      }
    */

    // --- ALTERNATIVE 2: Determine orientation by checking adjacent polygon's spatial location ---
    val (startNode, nextNode) = {
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

    // 2. Generate the required number of new nodes for the polygon.
    val sides = polygon.toSides
    val newNodesCount = sides - 2
    if (newNodesCount < 1) return (Vector(n2, n1, n2), Map.empty) // Special case for triangles

    val maxExistingNode = existingTiling.maxNode.toInt
    val newNodes = Vector.tabulate(newNodesCount)(i => Node(maxExistingNode + 1 + i))

    // 3. Iteratively calculate the coordinates of the new nodes.
    val alpha = polygon.alpha
    val coords = existingTiling.coordinates
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
  }

  /**
   * The incremental growth method. This is where the performance gain is.
   *
   * @param existingTiling The TilingAlt to grow from.
   * @param polygonToAdd   Information about the new polygon.
   * @param perimeterEdge  The perimeter edge the new polygon must be attached to.
   * @return               Either an error or the new, larger TilingAlt.
   */
  def addPolygon(existingTiling: TilingAlt, polygonToAdd: Polygon, perimeterEdge: Edge): Either[String, TilingAlt] =

    // 1. Perform VALIDATION using the existing tiling's data.
    //    - Check for overlaps, angle consistency, etc.
    //    - This is fast because all data is readily available.

    if !existingTiling.hasOnPerimeter(perimeterEdge) then
      return Left("Perimeter edge not found.")

    val newPolygon =
      ???

    // 2. Calculate the NEW properties INCREMENTALLY.
    val newEdges = existingTiling.edges ++ ???//polygonToAdd.toPolygonEdges
    val newPolygons = existingTiling.orientedPolygons :+ ??? // polygonToAdd

    // Incrementally update coordinates
    val newCoords = existingTiling.coordinates ++ calculateNewCoordinates(existingTiling, ??? /*polygonToAdd*/)

    // Incrementally update the perimeter
    val newPerimeter = updatePerimeter(existingTiling.perimeter, ??? /*polygonToAdd*/)

    // 3. Create the new immutable instance using the private constructor or copy.
    //    All data passed in is guaranteed to be consistent by our logic.
    Right(
      existingTiling.copy(
        edges = newEdges.distinct,
        orientedPolygons = newPolygons,
        perimeter = newPerimeter,
        coordinates = newCoords
      )
    )

  def removePolygon(existingTiling: TilingAlt, polygonToRemove: Vector[Node]): Either[String, TilingAlt] =
    ???

  // --- Helper methods for incremental calculation ---

  private def isValidAddition(tiling: TilingAlt, poly: Vector[Node]): Boolean =
    // Perform fast validation checks here
    true

  private def calculateNewCoordinates(tiling: TilingAlt, poly: Vector[Node]): Coords =
    // Logic to calculate coordinates for ONLY the new nodes in `poly`,
    // relative to the existing coordinates in `tiling.coordinates`.
    Map.empty // placeholder

  private def updatePerimeter(currentPerimeter: Vector[Node], poly: Vector[Node]): Vector[Node] =
    // The most complex part:
    // 1. Find edges shared between currentPerimeter and poly.
    // 2. Remove shared edges from the perimeter.
    // 3. Add the new, outward-facing edges from poly to the perimeter.
    // 4. Re-order the resulting edges to form the new RingPath.
    currentPerimeter // placeholder
