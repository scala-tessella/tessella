package io.github.scala_tessella.tessella

import utility.Utils.{compareElems, toCouple}
import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO}

import scala.math.Ordered.orderingToOrdered
//import math.geom2d.Point2D
//import math.geom2d.line.LineSegment2D

import scala.annotation.targetName

/** Planar geometry simplified toolbox */
object Geometry extends Accuracy:

  val ACCURACY = 1.0E-12

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
    val TAU: Radian = Radian(6.283185307179586)
    val TAU_2: Radian = Radian(Math.PI)
    val TAU_3: Radian = TAU / 3
    val TAU_4: Radian = Radian(1.5707963267948966)
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

  /** A point in the plane defined by its 2 Cartesian coordinates x and y */
  case class Point(x: Double, y: Double):

//    def toPoint2D: Point2D =
//      Point2D(x, y)

    /** Sum of two points */
    def plus(that: Point): Point =
      Point(this.x + that.x, this.y + that.y)

    /** Difference of two points */
    def minus(that: Point): Point =
      Point(this.x - that.x, this.y - that.y)

    private def rotate(theta: Radian): Point =
      val cot: Double =
        Math.cos(theta)
      val sit: Double =
        Math.sin(theta)
      Point(x * cot - y * sit, x * sit + y * cot)

    /** Tests whether this `Point` is approximately equal to another, within given accuracy. */
    def almostEquals(that: Point, accuracy: Double = ACCURACY): Boolean =
      this.x.~=(that.x, accuracy) && this.y.~=(that.y, accuracy)

    /** New point moved by polar coordinates
     *
     * @param rho   distance
     * @param theta angle
     */
    def plusPolar(rho: Double)(theta: Radian): Point =
      plus(Point.createPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: Radian => Point =
      plusPolar(1)

    /** Calculates the horizontal angle between two points */
    def angleTo(other: Point): Radian =
      LineSegment(this, other).horizontalAngle

    /** New point moved to align with reference to two other points */
    def alignWithStart(first: Point, second: Point): Point =
      minus(first).rotate(Radian.TAU - first.angleTo(second))

    /** New point flipped vertically around the x-axis */
    def flipVertically: Point =
      Point(x, -y)

  private class ApproximatePointOrdering(precision: Double = ROUND_ACCURACY) extends Ordering[Point] with Accuracy:

    private def rounded(point: Point): (Long, Long) =
      (point.x.rounded(precision), point.y.rounded(precision))

    def compare(x: Point, y: Point): Int =
      rounded(x) compare rounded(y)

  object Point:

    /** Creates a point at origin */
    def apply(): Point =
      Point(0, 0)

    /** Creates a point from polar coordinates */
    def createPolar(rho: Double, theta: Radian): Point =
      Point(rho * Math.cos(theta), rho * Math.sin(theta))

    /** Computes the orientation of the 3 points:
     *  returns +1 is the path P0->P1->P2 turns Counter-Clockwise,
     *  -1 if the path turns Clockwise,
     *  and 0 if the point P2 is located on the line segment [P0 P1].
     *  Algorithm taken from Sedgewick.
     *
     * @param p0 the initial point
     * @param p1 the middle point
     * @param p2 the last point
     * @return +1, 0 or -1, depending on the relative position of the points
     */
    def ccw(p0: Point, p1: Point, p2: Point): Int =
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

  extension (points: Vector[Point])

    private def sortedCouples: Iterator[(Point, Point)] =
      points.sorted(ApproximatePointOrdering()).sliding(2).map(_.toCouple)

    private def almostEqualCouple: ((Point, Point)) => Boolean =
      _.almostEquals(_, ACCURACY)

    /** Checks if all points are all distinct in 2D space */
    def areAllDistinct: Boolean =
      !sortedCouples.exists(almostEqualCouple)

    /** Filters all points couples that are not distinct in 2D space */
    def almostEqualCouples: Iterator[(Point, Point)] =
      sortedCouples.filter(almostEqualCouple)

    /** Checks if sequentially almost equal to another sequence */
    def almostEquals(others: Vector[Point]): Boolean =
      points.compareElems(others)(almostEqualCouple)

  /** Line segment, defined as the set of points located between the two end points. */
  case class LineSegment(point1: Point, point2: Point):

    private val dx: Double =
      point2.x - point1.x

    private val dy: Double =
      point2.y - point1.y

//    private def toLineSegment2D: LineSegment2D =
//      LineSegment2D(point1.toPoint2D, point2.toPoint2D)

    /** Checks if the given point is approximately equal to one of the two edges of the line segment */
    def containsAtEdges(point: Point): Boolean =
      point.almostEquals(point1, ACCURACY) || point.almostEquals(point2, ACCURACY)

    /** Computes the horizontal angle of the line segment */
    def horizontalAngle: Radian =
      Radian((Math.atan2(dy, dx) + Radian.TAU) % Radian.TAU)

    /** Tests whether this `LineSegment` is approximately equal to another, within given accuracy. */
    def almostEquals(that: LineSegment, accuracy: Double = ACCURACY): Boolean =
      this.point1.almostEquals(that.point1, accuracy) && this.point2.almostEquals(that.point2, accuracy)

    def intersects(that: LineSegment): Boolean =
      val e1p1: Point =
        this.point1
      val e1p2: Point =
        this.point2
      val e2p1: Point =
        that.point1
      val e2p2: Point =
        that.point2
      val b1: Boolean =
        Point.ccw(e1p1, e1p2, e2p1) * Point.ccw(e1p1, e1p2, e2p2) <= 0
      val b2: Boolean =
        Point.ccw(e2p1, e2p2, e1p1) * Point.ccw(e2p1, e2p2, e1p2) <= 0
      b1 && b2

    /** Checks if intersecting with another segment, without touching the edge points */
    def lesserIntersects(that: LineSegment): Boolean =
      this.intersects(that) && !(this.containsAtEdges(that.point1) || this.containsAtEdges(that.point2))

    /** Finds the intersection point with another line segment */
    def intersection(that: LineSegment): Option[Point] =
      val p1x = this.point1.x
      val p1y = this.point1.y
      val p3x = that.point1.x
      val p3y = that.point1.y

      val thatDx = that.point2.x - that.point1.x
      val thatDy = that.point2.y - that.point1.y

      val commonDenominator = this.dx * thatDy - this.dy * thatDx

      if commonDenominator.~=(0.0, ACCURACY) then
        // Lines are parallel or collinear.
        // A more advanced implementation could check for overlapping collinear segments here.
        None
      else
        // Parameter t for this segment: P_intersect = this.point1 + t * (this.dx, this.dy)
        val tNominator = (p1y - p3y) * thatDx - (p1x - p3x) * thatDy
        val t = tNominator / commonDenominator

        // Parameter u for that segment: P_intersect = that.point1 + u * (thatDx, thatDy)
        // uNominator derived from standard line intersection formulas:
        // u_num = this.dx * (p1y - p3y) - this.dy * (p1x - p3x)
        val uNominator = this.dx * (p1y - p3y) - this.dy * (p1x - p3x)
        val u = uNominator / commonDenominator

        // Check if parameters t and u are within [0, 1] range (with accuracy tolerance)
        if t >= (0.0 - ACCURACY) && t <= (1.0 + ACCURACY) &&
          u >= (0.0 - ACCURACY) && u <= (1.0 + ACCURACY) then
          val intersectX = p1x + t * this.dx
          val intersectY = p1y + t * this.dy
          Some(Point(intersectX, intersectY))
        else
          // Lines intersect, but not on both segments
          None

    /** Checks if at least one endpoint is contained in the given box */
    def hasEndpointIn(box: Box): Boolean =
      box.contains(point1) || box.contains(point2)

  extension (lines: Iterable[LineSegment])

    /** Checks if sequentially almost equal to another sequence */
    @targetName("almostEq")
    def almostEquals(others: Iterable[LineSegment]): Boolean =
      lines.compareElems(others)(_.almostEquals(_, LESSER_ACCURACY))

    /** Checks if the two sequences of segments are intersecting */
    def lesserIntersects(other: Iterable[LineSegment]): Boolean =
      lines.exists(line => other.exists(line.lesserIntersects))

  /** Bounds of a shape. */
  case class Box(x0: Double, x1: Double, y0: Double, y1: Double):

    def contains(point: Point): Boolean =
      if point.x < x0 then false
      else if point.y < y0 then false
      else if point.x > x1 then false
      else !(point.y > y1)

    def enlarge(d: Double): Box =
      Box(x0 - d, x1 +d, y0 - d, y1 + d)

    def width: Double =
      x1 - x0

    def height: Double =
      y1 - y0

  /** Represents a polygonal domain whose boundary is a single closed polyline. */
  class SimplePolygon(vertices: List[Point]):

    /** Gets an ordered sequence of vertices */
    val getVertices: List[Point] =
      vertices

    private def edges: Vector[LineSegment] =
      vertices.slidingO(2).map(_.toCouple).toVector.map(LineSegment(_, _))

    private def edgesCombinations: Iterator[(Index, Index)] =
      val length: Int =
        vertices.size
      for
        i1 <- (0 until length).iterator
        i2 <- (0 until length).iterator
        // avoid checking with self, already checked and adjacent edges
        if i1 > i2 + 1 && i1 != (if i2 == 0 then length else i2) - 1
      yield (i1, i2)

    /** Checks if the polygon is self-intersecting */
    def isSelfIntersecting: Boolean =
      edgesCombinations.exists((i1, i2) => edges(i1).intersects(edges(i2)))

    /** Filters the intersecting sides */
    def intersectingSides: Iterator[(LineSegment, LineSegment)] =
      edgesCombinations
        .filter((i1, i2) => edges(i1).intersects(edges(i2)))
        .map((i1, i2) => (edges(i1), edges(i2)))

  /** Represents a regular polygon. */
  class RegularPolygon2D(vertices: List[Point]) extends SimplePolygon(vertices):

    /** Center of the polygon */
    def center(): Point =
      val size: Int =
        vertices.size
      Point(vertices.map(_.x).sum / size, vertices.map(_.y).sum / size)
