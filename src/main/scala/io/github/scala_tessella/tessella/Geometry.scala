package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.GeometryBase.*
import Topology.{Edge, Node}
import utility.Utils.{compareElems, mapValues2, toCouple}

import io.github.scala_tessella.ring_seq.RingSeq.Index
import math.geom2d.{Angle2D, Box2D}
//import math.geom2d.point.PointArray2D
//import math.geom2d.Point2D.createPolar
//import math.geom2d.Shape2D.ACCURACY
//import math.geom2d.line.LineSegment2D
import math.geom2d.polygon.SimplePolygon2D

import scala.annotation.targetName
import scala.jdk.CollectionConverters.*

/** Associations of node and spatial 2D coordinates */
type Coords = Map[Node, Point9D]

/** Methods to help the spatial representation of a tiling */
object Geometry extends Accuracy:

  /** Standard unit of angular measure */
  opaque type Radian = Double

  /** Companion object for [[Radian]] */
  object Radian:

    /** Create a [[Radian]] from a `Double` */
    def apply(d: Double): Radian =
      d

    /**
     * @see [[https://tauday.com/]]
     */
    val TAU: Radian = Radian(Angle2D.M_2PI)
    val TAU_2: Radian = Radian(Angle2D.M_PI)
    val TAU_3: Radian = TAU / 3
    val TAU_4: Radian = Radian(Angle2D.M_PI_2)
    val TAU_6: Radian = TAU_2 / 3

  extension (r: Radian)

    /** @return the underlying `Double` */
    def toDouble: Double =
      r

    @targetName("plus")
    def +(that: Radian): Radian =
      r + that

    @targetName("minus")
    def -(that: Radian): Radian =
      r - that

    @targetName("times")
    def *(i: Int): Radian =
      r * Radian(i)

    @targetName("divide")
    def /(i: Int): Radian =
      r / Radian(i)

//  extension (point: Point2D)
//
//    /** Converts to rounded `Long`s */
//    def rounded: (Long, Long) =
//      (point.x.rounded(), point.y.rounded())
//
//    /** New point moved by polar coordinates
//     *
//     * @param rho   distance
//     * @param theta angle
//     */
//    def plusPolar(rho: Double)(theta: Radian): Point2D =
//      point.plus(Point2D(createPolar(rho, theta)))
//
//    /** New point moved by distance 1.0 */
//    def plusPolarUnit: Radian => Point2D =
//      plusPolar(1)
//
//    /** Calculates the horizontal angle between two points */
//    def angleTo(other: Point2D): Radian =
//      Radian(LineSegment2D(point, other).horizontalAngle)
//
//    /** New point moved to align with reference to two other points */
//    def alignWithStart(first: Point2D, second: Point2D): Point2D =
//      point.minus(first).rotate(Radian.TAU - first.angleTo(second))
//
//    /** New point flipped vertically around the x-axis */
//    def flipVertically: Point2D =
//      Point2D(point.x, -point.y)

//  extension (points: Vector[Point2D])
//
//    private def sortedCouples: Iterator[(Point2D, Point2D)] =
//      points.sortBy(rounded).sliding(2).map(_.toCouple)
//
//    private def almostEqualCouple: ((Point2D, Point2D)) => Boolean =
//      _.almostEquals(_, ACCURACY)
//
//    /** Checks if all points are all distinct in 2D space */
//    def areAllDistinct: Boolean =
//      !sortedCouples.exists(almostEqualCouple)
//
//    /** Filters all points couples that are not distinct in 2D space */
//    def almostEqualCouples: Iterator[(Point2D, Point2D)] =
//      sortedCouples.filter(almostEqualCouple)
//
//    /** Checks if sequentially almost equal to another sequence */
//    def almostEquals(others: Vector[Point2D]): Boolean =
//      points.compareElems(others)(almostEqualCouple)

//  extension (line: LineSegment2D)
//
//    /** Checks if intersecting with another segment, without touching the edge points */
//    def lesserIntersects(other: LineSegment2D): Boolean =
//      LineSegment2D.intersects(line, other) && !(line.contains(other.firstPoint) || line.contains(other.lastPoint))
//
//    /** Checks if at least one endpoint is contained in the given box */
//    def hasEndpointIn(box: Box2D): Boolean =
//      box.contains(line.firstPoint) || box.contains(line.lastPoint)

  extension (polygon: SimplePolygon2D)

    private def sides: Vector[LineSegment9D] =
      polygon.edges().asScala.toVector.map(LineSegment9D.fromLineSegment2D)

    private def edgesCombinations: Iterator[(Index, Index)] =
      val length = polygon.edgeNumber()
      for
        i1 <- (0 until length).iterator
        i2 <- (0 until length).iterator
        // avoid checking with self, already checked and adjacent edges
        if i1 > i2 + 1 && i1 != (if i2 == 0 then length else i2) - 1
      yield (i1, i2)

    /** Checks if the polygon is self intersecting */
    def isSelfIntersecting: Boolean =
      edgesCombinations.exists((i1, i2) => LineSegment9D.intersects(sides(i1), sides(i2)))

    /** Filters the intersecting sides */
    def intersectingSides: Iterator[(LineSegment9D, LineSegment9D)] =
      edgesCombinations
        .filter((i1, i2) => LineSegment9D.intersects(sides(i1), sides(i2)))
        .map((i1, i2) => (sides(i1), sides(i2)))

//  extension (lines: Iterable[LineSegment2D])
//
//    /** Checks if sequentially almost equal to another sequence */
//    @targetName("almostEq")
//    def almostEquals(others: Iterable[LineSegment2D]): Boolean =
//      lines.compareElems(others)(_.almostEquals(_, LESSER_ACCURACY))
//
//    /** Checks if the two sequences of segments are intersecting */
//    def lesserIntersects(other: Iterable[LineSegment2D]): Boolean =
//      lines.exists(line => other.exists(line.lesserIntersects))

  /** Spatial coordinates for the first two nodes of a [[Tiling]] */
  val startingCoords: Coords =
    Map(Node(1) -> Point9D(0, 0), Node(2) -> Point9D(1, 0))

  extension (coords: Coords)

    /** New coordinates guaranteeing that the third node of a [[Tiling]] has always a positive y value */
    def flipVertically: Coords =
      coords.get(Node(3)) match
        case Some(point) if point.y < 0 => coords.mapValues2(_.flipVertically)
        case _                          => coords

    /** New coordinates aligned with starting */
    def alignWithStart: Coords =
      (for
        one <- coords.get(Node(1))
        two <- coords.get(Node(2))
      yield coords.mapValues2(_.alignWithStart(one, two)))
        .map(_.flipVertically).getOrElse(coords)

  extension (edge: Edge)

    /** Creates a `LineSegment2D` of the given coordinates */
    def toSegment(coords: Coords): LineSegment9D =
      LineSegment9D(coords(edge.lesserNode), coords(edge.greaterNode))

  extension (edges: List[Edge])

    /** Creates a sequence of `LineSegment2D` of the given coordinates */
    def toSegments(coords: Coords): List[LineSegment9D] =
      edges.map(_.toSegment(coords))

//    /** @deprecated seems that .boundingBox doesn't work correctly in some cases */
//    def toBoxOld(coords: Coords): Box2D =
//      PointArray2D(edges.nodes.map(coords).asJava).boundingBox

    /** Creates a bounding `Box2D` containing all edges with the given coordinates
     *
     * @param coords      map with the points of each node
     * @param enlargement extra space at each side
     */
    def toBox(coords: Coords, enlargement: Double = 0.0): Box2D =
      val points: List[Point9D] =
        edges.nodes.map(coords)
      val xs: List[Double] =
        points.map(_.x)
      val ys: List[Double] =
        points.map(_.y)
      Box2D(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)
