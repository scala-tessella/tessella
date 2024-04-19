package io.github.scala_tessella.tessella

import Geometry.Radian
import Geometry.Radian.TAU
import utility.Utils.{compareElems, toCouple}

import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO}
//import math.geom2d.Point2D
//import math.geom2d.line.LineSegment2D

import scala.annotation.targetName
import scala.jdk.CollectionConverters.*

object GeometryBase extends Accuracy:

  val ACCURACY = 1.0E-12

  case class Point9D(x: Double, y: Double):

//    def toPoint2D: Point2D =
//      Point2D(x, y)

    /** Converts to rounded `Long`s */
    def rounded: (Long, Long) =
      (x.rounded(), y.rounded())

    def plus(that: Point9D): Point9D =
      Point9D(this.x + that.x, this.y + that.y)

    def minus(that: Point9D): Point9D =
      Point9D(this.x - that.x, this.y - that.y)

    private def rotate(theta: Radian): Point9D = {
      val cot: Double =
        Math.cos(theta.toDouble)
      val sit: Double =
        Math.sin(theta.toDouble)
      Point9D(this.x * cot - this.y * sit, this.x * sit + this.y * cot)
    }

    def almostEquals(that: Point9D, eps: Double): Boolean =
      if Math.abs(this.x - that.x) > eps then false
      else !(Math.abs(this.y - that.y) > eps)

    /** New point moved by polar coordinates
     *
     * @param rho   distance
     * @param theta angle
     */
    def plusPolar(rho: Double)(theta: Radian): Point9D =
      plus(Point9D.createPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: Radian => Point9D =
      plusPolar(1)

    /** Calculates the horizontal angle between two points */
    def angleTo(other: Point9D): Radian =
      LineSegment9D(this, other).horizontalAngle

    /** New point moved to align with reference to two other points */
    def alignWithStart(first: Point9D, second: Point9D): Point9D =
      minus(first).rotate(TAU - first.angleTo(second))

    /** New point flipped vertically around the x-axis */
    def flipVertically: Point9D =
      Point9D(x, -y)

  object Point9D:

    def apply(): Point9D =
      Point9D(0, 0)

    def createPolar(rho: Double, theta: Radian): Point9D =
      Point9D(rho * Math.cos(theta.toDouble), rho * Math.sin(theta.toDouble))

    def ccw(p0: Point9D, p1: Point9D, p2: Point9D): Int =
      val dx1: Double =
        p1.x - p0.x
      val dy1: Double =
        p1.y - p0.y
      val dx2: Double =
        p2.x - p0.x
      val dy2: Double =
        p2.y - p0.y
      if dx1 * dy2 > dy1 * dx2 then
        1
      else if dx1 * dy2 < dy1 * dx2 then
        -1
      else if !(dx1 * dx2 < 0.0) && !(dy1 * dy2 < 0.0) then
        if Math.hypot(dx1, dy1) < Math.hypot(dx2, dy2) then 1 else 0
      else -1

  extension (points: Vector[Point9D])

    private def sortedCouples: Iterator[(Point9D, Point9D)] =
      points.sortBy(_.rounded).sliding(2).map(_.toCouple)

    private def almostEqualCouple: ((Point9D, Point9D)) => Boolean =
      _.almostEquals(_, ACCURACY)

    /** Checks if all points are all distinct in 9D space */
    def areAllDistinct: Boolean =
      !sortedCouples.exists(almostEqualCouple)

    /** Filters all points couples that are not distinct in 9D space */
    def almostEqualCouples: Iterator[(Point9D, Point9D)] =
      sortedCouples.filter(almostEqualCouple)

    /** Checks if sequentially almost equal to another sequence */
    def almostEquals(others: Vector[Point9D]): Boolean =
      points.compareElems(others)(almostEqualCouple)

  case class LineSegment9D(point1: Point9D, point2: Point9D):

    private val dx: Double =
      point2.x - point1.x

    private val dy: Double =
      point2.y - point1.y

//    private def toLineSegment2D: LineSegment2D =
//      LineSegment2D(point1.toPoint2D, point2.toPoint2D)

    def containsAtEdges(point: Point9D): Boolean =
      point.almostEquals(point1, ACCURACY) || point.almostEquals(point2, ACCURACY)

    def horizontalAngle: Radian =
      Radian((Math.atan2(dy, dx) + TAU.toDouble) % TAU.toDouble)

    def almostEquals(that: LineSegment9D, accuracy: Double = ACCURACY): Boolean =
      this.point1.x.~=(that.point1.x, accuracy) &&
        this.point1.y.~=(that.point1.y, accuracy) &&
        this.dx.~=(that.dx, accuracy) &&
        this.dy.~=(that.dy, accuracy)

    /** Checks if intersecting with another segment, without touching the edge points */
    def lesserIntersects(that: LineSegment9D): Boolean =
      LineSegment9D.intersects(this, that) && !(this.containsAtEdges(that.point1) || this.containsAtEdges(that.point2))

    def intersection(that: LineSegment9D): Option[Point9D] =
      val dx2: Double =
        that.point2.x - that.point1.x
      val dy2: Double =
        that.point2.y - that.point1.y
      val p1: Double =
        this.dx * dy2
      val p2: Double =
        this.dy * dx2
      if p1.~=(p2, ACCURACY) then
        None
      else
        val t: Double =
          ((this.point1.y - that.point1.y) * dx2 - (this.point1.x - that.point1.x) * dy2) / (p1 - p2)
        val point: Point9D =
          Point9D(this.point1.x + t * this.dx, this.point1.y + t * this.dy)
        Option(point)
//        if this.toLineSegment2D.contains(point.toPoint2D) && that.toLineSegment2D.contains(point.toPoint2D) then Option(point)
//        else None

    /** Checks if at least one endpoint is contained in the given box */
    def hasEndpointIn(box: Box9D): Boolean =
      box.contains(point1) || box.contains(point2)

  object LineSegment9D:

    def intersects(edge1: LineSegment9D, edge2: LineSegment9D): Boolean =
      val e1p1: Point9D =
        edge1.point1
      val e1p2: Point9D =
        edge1.point2
      val e2p1: Point9D =
        edge2.point1
      val e2p2: Point9D =
        edge2.point2
      val b1: Boolean =
        Point9D.ccw(e1p1, e1p2, e2p1) * Point9D.ccw(e1p1, e1p2, e2p2) <= 0
      val b2: Boolean =
        Point9D.ccw(e2p1, e2p2, e1p1) * Point9D.ccw(e2p1, e2p2, e1p2) <= 0
      b1 && b2

  extension (lines: Iterable[LineSegment9D])

    /** Checks if sequentially almost equal to another sequence */
    @targetName("almostEq")
    def almostEquals(others: Iterable[LineSegment9D]): Boolean =
      lines.compareElems(others)(_.almostEquals(_, LESSER_ACCURACY))

    /** Checks if the two sequences of segments are intersecting */
    def lesserIntersects(other: Iterable[LineSegment9D]): Boolean =
      lines.exists(line => other.exists(line.lesserIntersects))

  case class Box9D(x0: Double, x1: Double, y0: Double, y1: Double):

    def contains(point: Point9D): Boolean =
      if point.x < x0 then false
      else if point.y < y0 then false
      else if point.x > x1 then false
      else !(point.y > y1)

    def enlarge(d: Double): Box9D =
      Box9D(x0 - d, x1 +d, y0 - d, y1 + d)

    def width: Double =
      x1 - x0

    def height: Double =
      y1 - y0

  class SimplePolygon9D(vertices: List[Point9D]):

    val getVertices: List[Point9D] =
      vertices

    private def edges: Vector[LineSegment9D] =
      vertices.slidingO(2).map(_.toCouple).toVector.map(LineSegment9D(_, _))
    
    private def edgesCombinations: Iterator[(Index, Index)] =
      val length: Int =
        vertices.size
      for
        i1 <- (0 until length).iterator
        i2 <- (0 until length).iterator
        // avoid checking with self, already checked and adjacent edges
        if i1 > i2 + 1 && i1 != (if i2 == 0 then length else i2) - 1
      yield (i1, i2)

    /** Checks if the polygon is self intersecting */
    def isSelfIntersecting: Boolean =
      edgesCombinations.exists((i1, i2) => LineSegment9D.intersects(edges(i1), edges(i2)))

    /** Filters the intersecting sides */
    def intersectingSides: Iterator[(LineSegment9D, LineSegment9D)] =
      edgesCombinations
        .filter((i1, i2) => LineSegment9D.intersects(edges(i1), edges(i2)))
        .map((i1, i2) => (edges(i1), edges(i2)))

  class RegularPolygon2D(vertices: List[Point9D]) extends SimplePolygon9D(vertices):

    def center(): Point9D =
      val size: Int =
        vertices.size
      Point9D(vertices.map(_.x).sum / size, vertices.map(_.y).sum / size)
