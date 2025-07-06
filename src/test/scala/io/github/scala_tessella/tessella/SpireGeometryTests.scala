package io.github.scala_tessella.tessella

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import spire.math.{Rational, Real}
import io.github.scala_tessella.tessella.SpireGeometry.*
import io.github.scala_tessella.tessella.SpireGeometry.SpireRadian.*

class SpireGeometrySpec extends AnyFlatSpec with should.Matchers:

  "An AngleDegree" should "be created from a Rational" in {
    AngleDegree(Rational(90)).toRational shouldBe Rational(90)
  }

  it should "be convertible to SpireRadian" in {
    AngleDegree(Rational(180)).toSpireRadian.almostEquals(SpireRadian(Real.pi)) shouldBe true
  }

  it should "be invertible" in {
    AngleDegree(Rational(45)).inverted.toRational shouldBe Rational(-45)
  }

  it should "support addition" in {
    (AngleDegree(Rational(90)) + AngleDegree(Rational(90))).toRational shouldBe Rational(180)
  }

  it should "support subtraction" in {
    (AngleDegree(Rational(180)) - AngleDegree(Rational(90))).toRational shouldBe Rational(90)
  }

  "A SpireRadian" can "be created as an opaque type" in {
    SpireRadian(0.0).toReal shouldBe Real(0.0)
  }

  def useSpireRadian(radian: SpireRadian): Boolean = true

  "A function" must "accept a SpireRadian param type" in {
    "useSpireRadian(SpireRadian(1.0))" should compile
  }

  it must "NOT accept a raw Real or Double" in {
    "useSpireRadian(Real(1.0))" shouldNot compile
    "useSpireRadian(1.0)" shouldNot compile
  }

  it can "be added to another" in {
    (SpireRadian(1.0) + SpireRadian(3.0)).toReal shouldBe Real(4.0)
  }

  it can "be subtracted from another" in {
    (SpireRadian(3.0) - SpireRadian(1.0)).toReal shouldBe Real(2.0)
  }

  it can "be multiplied by an integer" in {
    (SpireRadian(3.0) * 2).toReal shouldBe Real(6.0)
  }

  it can "be divided by an integer" in {
    (SpireRadian(2.0) / 2).toReal shouldBe Real(1.0)
  }

  val p1: SpirePoint = SpirePoint(Real(1), Real(-1))
  val p2: SpirePoint = SpirePoint(Real(4), Real(3))
  val origin: SpirePoint = SpirePoint()

  "A SpirePoint" can "be created at the origin" in {
    origin.x shouldBe Real(0)
    origin.y shouldBe Real(0)
  }

  it can "be added to another point" in {
    p1.plus(p2) shouldBe SpirePoint(Real(5), Real(2))
  }

  it can "be subtracted from another point" in {
    p2.minus(p1) shouldBe SpirePoint(Real(3), Real(4))
  }

  it can "be rotated" in {
    val point = SpirePoint(Real(1), Real(0))
    val rotated = point.rotate(TAU_4) // 90 degrees
    rotated.almostEquals(SpirePoint(Real(0), Real(1))) shouldBe true
  }

  it can "be checked for approximate equality" in {
    val pAlmost = SpirePoint(Real("0.9999999999999999"), Real("-1.0"))
    p1.almostEquals(pAlmost) shouldBe true
    p1.almostEquals(pAlmost, 1e-17) shouldBe false
  }

  it can "be moved by polar coordinates" in {
    val moved = origin.plusPolar(Real(2))(TAU_4) // move by 2 units at 90 degrees
    moved.almostEquals(SpirePoint(Real(0), Real(2))) shouldBe true
  }

  it can "be moved by 1 unit in a polar direction" in {
    val moved = origin.plusPolarUnit(TAU_2) // move by 1 unit at 180 degrees
    moved.almostEquals(SpirePoint(Real(-1), Real(0))) shouldBe true
  }

  it can "calculate the angle to another point" in {
    origin.angleTo(SpirePoint(Real(1), Real(0))).toReal.toDouble shouldBe 0.0
    origin.angleTo(SpirePoint(Real(0), Real(1))).almostEquals(TAU_4) shouldBe true
  }

  it can "calculate the distance to another point" in {
    SpirePoint(Real(0), Real(0)).distanceTo(SpirePoint(Real(3), Real(4))) shouldBe Real(5)
  }

  it can "be aligned with two other points" in {
    val p = SpirePoint(Real(2), Real(2))
    val first = SpirePoint(Real(1), Real(1))
    val second = SpirePoint(Real(2), Real(1))
    val aligned = p.alignWithStart(first, second)
    aligned.almostEquals(SpirePoint(Real(1), Real(1))) shouldBe true
  }

  it can "be flipped vertically" in {
    p1.flipVertically shouldBe SpirePoint(Real(1), Real(1))
  }

  "The SpirePoint companion object" can "create a point at the origin" in {
    SpirePoint() shouldBe SpirePoint(Real(0), Real(0))
  }

  it can "create a point from polar coordinates" in {
    val polar = SpirePoint.createPolar(Real(1), TAU_4)
    polar.almostEquals(SpirePoint(Real(0), Real(1))) shouldBe true
  }

  it should "correctly determine ccw" in {
    val p0 = SpirePoint(0, 0)
    val p1 = SpirePoint(1, 0)
    val p2 = SpirePoint(1, 1) // Counter-Clockwise
    val p3 = SpirePoint(1, -1) // Clockwise
    val p4 = SpirePoint(2, 0) // Collinear
    SpirePoint.ccw(p0, p1, p2) shouldBe 1
    SpirePoint.ccw(p0, p1, p3) shouldBe -1
    SpirePoint.ccw(p0, p1, p4) shouldBe 1
    SpirePoint.ccw(p0, p4, p1) shouldBe 0
  }

  "A SpireLineSegment" can "calculate its length" in {
    val segment = SpireLineSegment(SpirePoint(Real(1), Real(2)), SpirePoint(Real(4), Real(6)))
    segment.length shouldBe Real(5)
  }

  it can "calculate its horizontal angle" in {
    val segment = SpireLineSegment(SpirePoint(Real(0), Real(0)), SpirePoint(Real(1), Real(1)))
    segment.horizontalAngle.almostEquals(TAU_2 / 4) shouldBe true
  }

  val m: Matrix2x2 = Matrix2x2(Real(1), Real(2), Real(3), Real(4))

  "A Matrix2x2" can "calculate its determinant" in {
    m.determinant shouldBe Real(-2)
  }

  it can "calculate its inverse" in {
    val inv = m.inverse.get
    inv.m00 shouldBe Real(-2)
    inv.m01 shouldBe Real(1)
    inv.m10 shouldBe Real(1.5)
    inv.m11 shouldBe Real(-0.5)
  }

  it can "return None for inverse if determinant is zero" in {
    val singular = Matrix2x2(Real(1), Real(2), Real(2), Real(4))
    singular.inverse shouldBe None
  }

  it can "be multiplied by another matrix" in {
    val m2 = Matrix2x2(Real(2), Real(0), Real(1), Real(2))
    val result = m.multiply(m2)
    result shouldBe Matrix2x2(Real(4), Real(4), Real(10), Real(8))
  }

  it can "transform a point" in {
    val p = SpirePoint(Real(1), Real(1))
    val transformed = m.transform(p)
    transformed shouldBe SpirePoint(Real(3), Real(7))
  }

  "The Matrix2x2 companion object" can "create a matrix from column vectors" in {
    val v1 = SpirePoint(Real(1), Real(3))
    val v2 = SpirePoint(Real(2), Real(4))
    Matrix2x2.fromColumns(v1, v2) shouldBe m
  }

  it can "find a transformation between vector pairs" in {
    val v1 = SpirePoint(1, 0)
    val v2 = SpirePoint(0, 1)
    val u1 = SpirePoint(2, 3)
    val u2 = SpirePoint(4, 5)
    val transform = Matrix2x2.findTransform(v1, v2, u1, u2).get
    transform.almostEquals(Matrix2x2.fromColumns(u1, u2)) shouldBe true
  }
