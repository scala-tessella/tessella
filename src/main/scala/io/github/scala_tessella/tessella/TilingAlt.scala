package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import TilingCoordinates.{Coords, pointsFrom}
import Topology.{Edge, Node, zeroNode}
import io.github.scala_tessella.tessella.Geometry.Radian.TAU_2
import io.github.scala_tessella.tessella.Geometry.{Point, Radian}

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
   * The incremental growth method. This is where the performance gain is.
   *
   * @param existingTiling The TilingAlt to grow from.
   * @param polygonToAdd   Information about the new polygon.
   * @return               Either an error or the new, larger TilingAlt.
   */
  def addPolygon(
                  existingTiling: TilingAlt,
                  polygonToAdd: Vector[Node] // Or whatever info is needed
                ): Either[String, TilingAlt] =

    // 1. Perform VALIDATION using the existing tiling's data.
    //    - Check for overlaps, angle consistency, etc.
    //    - This is fast because all data is readily available.
    if (!isValidAddition(existingTiling, polygonToAdd))
      return Left("Invalid addition: polygons would overlap.")

    // 2. Calculate the NEW properties INCREMENTALLY.
    val newEdges = existingTiling.edges ++ ???//polygonToAdd.toPolygonEdges
    val newPolygons = existingTiling.orientedPolygons :+ polygonToAdd

    // Incrementally update coordinates
    val newCoords = existingTiling.coordinates ++ calculateNewCoordinates(existingTiling, polygonToAdd)

    // Incrementally update the perimeter
    val newPerimeter = updatePerimeter(existingTiling.perimeter, polygonToAdd)

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
