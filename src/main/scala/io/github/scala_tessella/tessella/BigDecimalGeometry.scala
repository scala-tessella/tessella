package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.Geometry.Point
import io.github.scala_tessella.tessella.Topology.{Edge, Node}

import spire.implicits.*
import scala.annotation.targetName

/**
 * Planar geometry toolbox using Spire for precise calculations.
 *
 * This object provides an alternative to [[Geometry]], using Spire's numeric types like [[spire.math.BigDecimal]]
 * to avoid floating-point inaccuracies.
 */
object BigDecimalGeometry:

  val ACCURACY = 1.0E-12

//  opaque type AngleDegree = Rational
//
//  object AngleDegree:
//
//    def apply(r: Rational): AngleDegree =
//      r
//
//  extension (d: AngleDegree)
//
//    def toRational: Rational =
//      d
//
//    def toSpireRadian: SpireRadian =
//      BigDecimal(d.toReal) * BigDecimal.pi / 180
//
//    def inverted: AngleDegree =
//      -d
//
//    @targetName("plusDegree")
//    def +(that: AngleDegree): AngleDegree =
//      d.toRational + that
//
//    @targetName("minusDegree")
//    def -(that: AngleDegree): AngleDegree =
//      d.toRational - that

  /** Standard unit of angular measure, represented by a [[spire.math.BigDecimal]]. */
  opaque type BigRadian = BigDecimal

  /** Companion object for [[BigRadian]] */
  object BigRadian:
    /** Create a [[BigRadian]] from a `Double` */
    def apply(d: Double): BigRadian = BigDecimal(d)

    /** Create a [[BigRadian]] from a `BigDecimal` */
    def apply(b: BigDecimal): BigRadian = b

    /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
    val TAU: BigRadian = BigDecimal(spire.math.pi) * 2
    /** Pi, half of Tau. */
    val TAU_2: BigRadian = BigDecimal(spire.math.pi)
    val TAU_3: BigRadian = TAU / 3
    /** Half of Pi. */
    val TAU_4: BigRadian = BigDecimal(spire.math.pi) / 2
    val TAU_6: BigRadian = TAU_2 / 3

  extension (r: BigRadian)
    /** @return the underlying `BigDecimal` */
    def toBigDecimal: BigDecimal =
      r

    @targetName("plus")
    def +(that: BigRadian): BigRadian = r.toBigDecimal + that

    @targetName("minus")
    def -(that: BigRadian): BigRadian = r.toBigDecimal - that

    @targetName("times")
    def *(i: Int): BigRadian = r.toBigDecimal * i

    @targetName("divide")
    def /(i: Int): BigRadian = r.toBigDecimal / i

    /** Tests whether this `SpireRadian` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: BigRadian, accuracy: Double = ACCURACY): Boolean =
      (r - that).abs < BigDecimal(accuracy)

  /** A point in the plane defined by its 2 Cartesian coordinates x and y using [[spire.math.BigDecimal]]. */
  case class BigPoint(x: BigDecimal, y: BigDecimal):

    def toPoint: Point =
      Point(x.toDouble, y.toDouble)
      
    /** Sum of two points */
    def plus(that: BigPoint): BigPoint =
      BigPoint(this.x + that.x, this.y + that.y)

    /** Tests whether this `BigPoint` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: BigPoint, accuracy: Double = ACCURACY): Boolean =
      (this.x - that.x).abs < BigDecimal(accuracy) && (this.y - that.y).abs < BigDecimal(accuracy)

    /** New point moved by polar coordinates */
    def plusPolar(rho: BigDecimal)(theta: BigRadian): BigPoint =
      plus(BigPoint.createPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: BigRadian => BigPoint =
      plusPolar(BigDecimal(1.0))

    /** Calculates the horizontal angle of the vector from this point to another point. */
    def angleTo(other: BigPoint): BigRadian =
      BigLineSegment(this, other).horizontalAngle

    /** Calculates the distance to another point. */
    def distanceTo(other: BigPoint): BigDecimal =
      BigLineSegment(this, other).length

  object BigPoint:
    /** Creates a point at origin (0,0) */
    def apply(): BigPoint = BigPoint(BigDecimal(0), BigDecimal(0))

    /** Creates a point from polar coordinates */
    def createPolar(rho: BigDecimal, theta: BigRadian): BigPoint =
      BigPoint(rho * spire.math.cos(theta), rho * spire.math.sin(theta))

  /** A line segment in the plane defined by its 2 endpoints using [[spire.math.BigDecimal]]. */
  case class BigLineSegment(p1: BigPoint, p2: BigPoint):
    /** The length of the line segment. */
    lazy val length: BigDecimal =
      spire.math.sqrt((p2.x - p1.x).pow(2) + (p2.y - p1.y).pow(2))

    def midPoint: BigPoint =
      BigPoint((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)

    /** The horizontal angle of the line segment. */
    def horizontalAngle: BigRadian =
      spire.math.atan2(p2.y - p1.y, p2.x - p1.x)
    
  case class BigBox(x0: BigDecimal, x1: BigDecimal, y0: BigDecimal, y1: BigDecimal):

    def contains(point: BigPoint): Boolean =
      if point.x < x0 then false
      else if point.y < y0 then false
      else if point.x > x1 then false
      else !(point.y > y1)

    def enlarge(r: BigDecimal): BigBox =
      BigBox(x0 - r, x1 + r, y0 - r, y1 + r)

  type BigCoords = Map[Node, BigPoint]

  extension (edges: List[Edge])

    /** Creates a bounding `Box` containing all edges with the given coordinates
     *
     * @param coords      map with the points of each node
     * @param enlargement extra space at each side
     */
    def toBox(coords: BigCoords, enlargement: BigDecimal = 0): BigBox =
      val points: List[BigPoint] =
        edges.nodes.map(coords)
      val xs: List[BigDecimal] =
        points.map(_.x)
      val ys: List[BigDecimal] =
        points.map(_.y)
      BigBox(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)

  extension (nodes: Vector[Node])

    def pointsFrom(angles: Map[Node, BigRadian]): Vector[BigPoint] =
      nodes.scanLeft((BigPoint(1, 0), BigRadian.TAU_2))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + BigRadian.TAU_2)
      }).map((point, _) => point).tail
