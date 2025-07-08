package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.Geometry.*
import io.github.scala_tessella.tessella.Geometry.Radian.TAU_4
import io.github.scala_tessella.tessella.TilingCoordinates.*
import io.github.scala_tessella.tessella.Topology.{--, Edge, Node}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GeometryDriftSpec extends AnyFlatSpec with Helper with should.Matchers:
  
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
    val error = errorForNSides(100)
    println(s"Euclidean distance between start and end point for n=1000: $error")
    // the error is expected to be there
    error should be > 1.0E-13
  }

  it should "have a bigger error for a greater number of sides" in {
    val error100 = errorForNSides(100)
    val error1000 = errorForNSides(1000)
    val error10000 = errorForNSides(10000)
    println(s"Error for n=100: $error100")
    println(s"Error for n=1000: $error1000")
    println(s"Error for n=10000: $error10000")
    error1000 should be > error100
    error10000 should be > error1000
  }
