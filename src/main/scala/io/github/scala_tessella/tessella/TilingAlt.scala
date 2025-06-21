package io.github.scala_tessella.tessella

import Geometry.{Box, Point, Radian}
import Geometry.Radian.TAU_2
import RegularPolygon.Polygon
import TilingCoordinates.{Coords, pointsFrom, toBox}
import Topology.{Edge, Node}

import io.github.scala_tessella.ring_seq.RingSeq.slidingO

import scala.collection.mutable

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
                                    polygon: Vector[Node],
                                    newCoords: Coords,
                                    perimeterCoords: Coords
                                  ): (Vector[Node], Coords) = {
    val substitutions = newCoords.toList.flatMap { case (newNode, newPoint) =>
      perimeterCoords.find { case (_, perimeterPoint) =>
        newPoint.almostEquals(perimeterPoint)
      }.map { case (perimeterNode, _) =>
        newNode -> perimeterNode
      }
    }.toMap

    if (substitutions.isEmpty) {
      (polygon, newCoords)
    } else {
      val mergedPolygon = polygon.map(node => substitutions.getOrElse(node, node))
      val mergedCoords = newCoords -- substitutions.keys
      (mergedPolygon, mergedCoords)
    }
  }

  private def updatePerimeter(currentPerimeter: Vector[Node], poly: Vector[Node]): Vector[Node] =
    val perimeterEdges = currentPerimeter.toEdgesO.toList
    val polyEdges = poly.toEdgesO.toList

    val sharedEdges = perimeterEdges.filter(edge => polyEdges.contains(edge))
    val remainingPerimeterEdges = perimeterEdges.filterNot(sharedEdges.contains)
    val newPolyEdges = polyEdges.filterNot(edge => sharedEdges.contains(edge))

    val allNewPerimeterEdges = remainingPerimeterEdges ++ newPolyEdges

    if (allNewPerimeterEdges.isEmpty) Vector.empty
    else {
      val adj = allNewPerimeterEdges.flatMap(e => List(e.lesserNode -> e.greaterNode, e.greaterNode -> e.lesserNode))
        .groupMap(_._1)(_._2)

      val path = mutable.ListBuffer.empty[Node]
      val start = allNewPerimeterEdges.head.lesserNode
      path += start
      var current = start
      var prev = start

      var next = adj(current).head
      path += next
      prev = current
      current = next

      while (current != start) {
        val candidates = adj(current)
        next = if (candidates.head == prev) candidates.last else candidates.head
        path += next
        prev = current
        current = next
      }
      path.init.toVector
    }
    
  /**
   * The incremental growth method. This is where the performance gain is.
   *
   * @param polygon        Information about the new polygon.
   * @param perimeterEdge  The perimeter edge the new polygon must be attached to.
   * @return Either an error or the new, larger TilingAlt.
   */
  def addPolygon(polygon: Polygon, perimeterEdge: Edge): Either[String, TilingAlt] =

    if !hasOnPerimeter(perimeterEdge) then
      return Left("Perimeter edge not found.")

    val (initialPolygon, additionalCoords) = calculateNewPolygonCoords(polygon, perimeterEdge)

    val perimeterCoords = coordinates.filter((node, _) => perimeter.contains(node))
    val (mergedPolygon, finalAdditionalCoords) =
      mergeCoincidentNodes(initialPolygon, additionalCoords, perimeterCoords)

    val additionalEdges = mergedPolygon.toEdgesO.toList.filterNot(edge => edge.pair._1 == edge.pair._2)

    val newEdges = (edges ++ additionalEdges).distinct
    val newPolygons = orientedPolygons :+ mergedPolygon
    val newCoords = coordinates ++ finalAdditionalCoords
    val newPerimeter = updatePerimeter(perimeter, mergedPolygon)

    Right(
      TilingAlt(
        newEdges,
        newPolygons,
        newPerimeter,
        newCoords
      )
    )

  def removePolygon(polygonPath: Vector[Node]): Either[String, TilingAlt] =
    ???

  /** Finds the 2D box */
  def toBox: Box =
    edges.toBox(coordinates)

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

  def fromTiling(tiling: Tiling): TilingAlt =
    TilingAlt(
      tiling.graphEdges,
      tiling.orientedPolygons.map(_.toPolygonPathNodes),
      tiling.perimeter.toRingNodes,
      tiling.coords
    )
