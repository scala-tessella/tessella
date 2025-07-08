package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.Geometry.*

import spire.implicits.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

case class PointBig(x: BigDecimal, y: BigDecimal):

  def plus(that: PointBig): PointBig =
    PointBig(this.x + that.x, this.y + that.y)

  def plusPolar(rho: BigDecimal)(theta: BigDecimal): PointBig =
    plus(PointBig.createPolar(rho, theta))

  def plusPolarUnit: BigDecimal => PointBig =
    plusPolar(1)

  def angleTo(other: PointBig): BigDecimal =
    val dx: BigDecimal =
      other.x - this.x
    val dy: BigDecimal =
      other.y - this.y
    spire.math.atan2(dy, dx)

  def distanceTo(other: PointBig): BigDecimal =
    val dx: BigDecimal =
      other.x - this.x
    val dy: BigDecimal =
      other.y - this.y
    spire.math.hypot(dx, dy)

object PointBig:

  def createPolar(rho: BigDecimal, theta: BigDecimal): PointBig =
    PointBig(rho * spire.math.cos(theta), rho * spire.math.sin(theta))

class GeometryDriftSpec extends AnyFlatSpec with Helper with should.Matchers:

  def errorBig(n: Int): BigDecimal =
    val exteriorAngle: BigDecimal = BigDecimal(Math.PI) * 2 / n
    val y: Double = -0.5 / Math.tan(Math.PI / n)
    val p0 = PointBig(-0.5, BigDecimal(y))
    val p1 = PointBig(0.5, BigDecimal(y))
    val (_, p_n) = (2 to n).foldLeft((p0, p1))({
      case ((pPrev, pCurr), _) =>
        val angle = pPrev.angleTo(pCurr) + exteriorAngle
        val pNext = pCurr.plusPolarUnit(angle)
        //        println(s"pCurr: $pCurr, pNext: $pNext, angle: $angle")
        (pCurr, pNext)
    })
    p0.distanceTo(p_n)

  def errorForNSides(n: Int): Double =
    val exteriorAngle = Radian.TAU / n
    val y = -0.5 / Math.tan(Math.PI / n)
    val p0 = Point(-0.5, y)
    val p1 = Point(0.5, y)
    val (_, p_n) = (2 to n).foldLeft((p0, p1))({
      case ((pPrev, pCurr), _) =>
        val angle = pPrev.angleTo(pCurr) + exteriorAngle
        val pNext = pCurr.plusPolarUnit(angle)
//        println(s"pCurr: $pCurr, pNext: $pNext, angle: $angle")
        (pCurr, pNext)
    })
    p0.distanceTo(p_n)
  
  "A regular polygon calculated iteratively from its vertices" should
    "eventually not close, because of floating point drifting error" in {
    val error = errorForNSides(1000)
    println(s"Euclidean distance between start and end point for n=1000: $error")
    // the error is expected to be there
    error should be > 1.0E-13
  }

  it should "have the same with BigDecimal but less drifting" in {
    val error = errorBig(1000)
    println(s"Euclidean distance with BigDecimal between start and end point for n=1000: $error")
    // the error is expected to be there
    error should be < BigDecimal(1.0E-13)
  }
  
  it should "have a bigger error for a greater number of sides" in {
    val error100 = errorForNSides(100)
    val error1000 = errorForNSides(1000)
    val error10000 = errorForNSides(10000)
    val error100000 = errorForNSides(100000)
    println(s"Error for n=100: $error100")
    println(s"Error for n=1000: $error1000")
    println(s"Error for n=10000: $error10000")
    println(s"Error for n=100000: $error100000")
    error1000 should be > error100
    error10000 should be > error1000
    error100000 should be > error10000
  }

  it should "also with BigDecimal have a bigger error for a greater number of sides" in {
    val error100 = errorBig(100)
    val error1000 = errorBig(1000)
    val error10000 = errorBig(10000)
    val error100000 = errorBig(100000)
    println(s"BigDecimal error for n=100: $error100")
    println(s"BigDecimal error for n=1000: $error1000")
    println(s"BigDecimal error for n=10000: $error10000")
    println(s"BigDecimal error for n=100000: $error100000")
    error1000 should be > error100
    error10000 should be > error1000
    error100000 should be > error10000
  }
