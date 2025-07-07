package io.github.scala_tessella.tessella

import spire.math.{Real, Rational, Algebraic}
import spire.implicits.*
import scala.annotation.targetName

/**
 * Planar geometry toolbox using Spire for precise calculations.
 *
 * This object provides an alternative to [[Geometry]], using Spire's numeric types like [[spire.math.Real]]
 * to avoid floating-point inaccuracies.
 */
object SpireGeometry:

  val ACCURACY = 1.0E-12

  opaque type AngleDegree = Rational

  object AngleDegree:

    def apply(r: Rational): AngleDegree =
      r

  extension (d: AngleDegree)

    def toRational: Rational =
      d

    def toSpireRadian: SpireRadian =
      d.toReal * Real.pi / 180

    def inverted: AngleDegree =
      -d

    @targetName("plusDegree")
    def +(that: AngleDegree): AngleDegree =
      d.toRational + that

    @targetName("minusDegree")
    def -(that: AngleDegree): AngleDegree =
      d.toRational - that

  /** Standard unit of angular measure, represented by a [[spire.math.Real]]. */
  opaque type SpireRadian = Real

  /** Companion object for [[SpireRadian]] */
  object SpireRadian:
    /** Create a [[SpireRadian]] from a `Double` */
    def apply(d: Double): SpireRadian = Real(d)

    /** Create a [[SpireRadian]] from a `Real` */
    def apply(r: Real): SpireRadian = r

    /** Tau (2 * Pi), the circle constant. [[https://tauday.com/]] */
    val TAU: SpireRadian = Real.pi * 2
    /** Pi, half of Tau. */
    val TAU_2: SpireRadian = Real.pi
    val TAU_3: SpireRadian = TAU / 3
    /** Half of Pi. */
    val TAU_4: SpireRadian = Real.pi / 2
    val TAU_6: SpireRadian = TAU_2 / 3

  extension (r: SpireRadian)
    /** @return the underlying `Real` */
    def toReal: Real =
      r

    @targetName("plus")
    def +(that: SpireRadian): SpireRadian = r.toReal + that

    @targetName("minus")
    def -(that: SpireRadian): SpireRadian = r.toReal - that

    @targetName("times")
    def *(i: Int): SpireRadian = r.toReal * i

    @targetName("divide")
    def /(i: Int): SpireRadian = r.toReal / i

    /** Tests whether this `SpireRadian` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: SpireRadian, accuracy: Double = ACCURACY): Boolean =
      (r - that).abs < Real(accuracy)

  /** A point in the plane defined by its 2 Cartesian coordinates x and y using [[spire.math.Real]]. */
  case class SpirePoint(x: Real, y: Real):

    /** Sum of two points */
    def plus(that: SpirePoint): SpirePoint =
      SpirePoint(this.x + that.x, this.y + that.y)

    /** Difference of two points */
    def minus(that: SpirePoint): SpirePoint =
      SpirePoint(this.x - that.x, this.y - that.y)

    /** Rotates the point by a given angle around the origin. */
    def rotate(theta: SpireRadian): SpirePoint = {
      val cot = spire.math.cos(theta)
      val sit = spire.math.sin(theta)
      SpirePoint(x * cot - y * sit, x * sit + y * cot)
    }

    /** Tests whether this `SpirePoint` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: SpirePoint, accuracy: Double = ACCURACY): Boolean =
      (this.x - that.x).abs < Real(accuracy) && (this.y - that.y).abs < Real(accuracy)

    /** New point moved by polar coordinates */
    def plusPolar(rho: Real)(theta: SpireRadian): SpirePoint =
      plus(SpirePoint.createPolar(rho, theta))

    /** New point moved by distance 1.0 */
    def plusPolarUnit: SpireRadian => SpirePoint =
      plusPolar(Real(1.0))

    /** Calculates the horizontal angle of the vector from this point to another point. */
    def angleTo(other: SpirePoint): SpireRadian =
      SpireLineSegment(this, other).horizontalAngle

    /** Calculates the distance to another point. */
    def distanceTo(other: SpirePoint): Real =
      SpireLineSegment(this, other).length

    /** New point moved to align with reference to two other points */
    def alignWithStart(first: SpirePoint, second: SpirePoint): SpirePoint =
      minus(first).rotate(SpireRadian.TAU - first.angleTo(second))

    /** New point flipped vertically around the x-axis */
    def flipVertically: SpirePoint =
      SpirePoint(x, -y)

  object SpirePoint:
    /** Creates a point at origin (0,0) */
    def apply(): SpirePoint = SpirePoint(Real(0), Real(0))

    /** Creates a point from polar coordinates */
    def createPolar(rho: Real, theta: SpireRadian): SpirePoint =
      SpirePoint(rho * spire.math.cos(theta), rho * spire.math.sin(theta))

    /**
     * Computes the orientation of the 3 points (p0, p1, p2).
     * @return 1 if the path p0->p1->p2 turns Counter-Clockwise, -1 if Clockwise,
     *         and 0 if the points are collinear and p2 is on the segment [p0, p1].
     *         The collinear logic is adapted from the original `Geometry.ccw` method.
     */
    def ccw(p0: SpirePoint, p1: SpirePoint, p2: SpirePoint): Int =
      val dx1 = p1.x - p0.x
      val dy1 = p1.y - p0.y
      val dx2 = p2.x - p0.x
      val dy2 = p2.y - p0.y
      val crossProduct = dx1 * dy2 - dy1 * dx2
      if crossProduct > 0 then 1
      else if crossProduct < 0 then -1
      else
        if dx1 * dx2 >= 0 && dy1 * dy2 >= 0 then
          if (dx1 * dx1 + dy1 * dy1) < (dx2 * dx2 + dy2 * dy2) then 1 else 0
        else -1

  /** A line segment in the plane defined by its 2 endpoints using [[spire.math.Real]]. */
  case class SpireLineSegment(p1: SpirePoint, p2: SpirePoint):
    /** The length of the line segment. */
    lazy val length: Real =
      spire.math.sqrt((p2.x - p1.x).pow(2) + (p2.y - p1.y).pow(2))

    /** The horizontal angle of the line segment. */
    def horizontalAngle: SpireRadian =
      spire.math.atan2(p2.y - p1.y, p2.x - p1.x)

  /** 2x2 matrix for linear transformations */
  case class Matrix2x2(m00: Real, m01: Real, m10: Real, m11: Real):
    /** Determinant of the matrix */
    def determinant: Real =
      m00 * m11 - m01 * m10

    /** Inverse of the matrix, if it exists */
    def inverse: Option[Matrix2x2] =
      val det = determinant
      if det.abs < Real(ACCURACY) then None
      else
        val invDet = Real(1) / det
        Some(Matrix2x2(
          m11 * invDet, -m01 * invDet,
          -m10 * invDet, m00 * invDet
        ))

    /** Matrix multiplication with another 2x2 matrix */
    def multiply(other: Matrix2x2): Matrix2x2 =
      Matrix2x2(
        m00 * other.m00 + m01 * other.m10,
        m00 * other.m01 + m01 * other.m11,
        m10 * other.m00 + m11 * other.m10,
        m10 * other.m01 + m11 * other.m11
      )

    /** Transform a point by this matrix */
    def transform(p: SpirePoint): SpirePoint =
      SpirePoint(
        m00 * p.x + m01 * p.y,
        m10 * p.x + m11 * p.y
      )

    /** Tests whether this `Matrix2x2` is approximately equal to another, within a given accuracy. */
    def almostEquals(that: Matrix2x2, accuracy: Double = ACCURACY): Boolean =
      (this.m00 - that.m00).abs < Real(accuracy) &&
        (this.m01 - that.m01).abs < Real(accuracy) &&
        (this.m10 - that.m10).abs < Real(accuracy) &&
        (this.m11 - that.m11).abs < Real(accuracy)

  object Matrix2x2:
    /** Creates a matrix from two column vectors. */
    def fromColumns(c1: SpirePoint, c2: SpirePoint): Matrix2x2 =
      Matrix2x2(c1.x, c2.x, c1.y, c2.y)

    /** Finds the transformation matrix that maps vector pair (v1, v2) to (u1, u2). */
    def findTransform(v1: SpirePoint, v2: SpirePoint, u1: SpirePoint, u2: SpirePoint): Option[Matrix2x2] =
      val m1 = Matrix2x2.fromColumns(v1, v2)
      val m2 = Matrix2x2.fromColumns(u1, u2)
      m1.inverse.map(m2.multiply)
