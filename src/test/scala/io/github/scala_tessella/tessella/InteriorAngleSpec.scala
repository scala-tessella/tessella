package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU
import RegularPolygon.Polygon

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Test.check
import org.scalatest.flatspec.*

class InteriorAngleSpec extends AnyFlatSpec {

  def interiorAngle(sides: Int): Double =
    Polygon(sides).alpha.toDouble

  "The only monohedral tilings by regular polygons" must "be (3₆), (4₄) and (6₃)" in {

    def isFillingWithoutRemainder(sides: Int): Boolean =
      (TAU.toDouble % interiorAngle(sides)) ~= 0.0

    check(forAll(Gen.choose(3, 100))({
      case ok if List(3, 4, 6).contains(ok) =>
        val times = TAU.toDouble / interiorAngle(ok)
        isFillingWithoutRemainder(ok) && (ok match
          case 3 => times ~= 6.0
          case 4 => times ~= 4.0
          case 6 => times ~= 3.0
        )
      case ko => !isFillingWithoutRemainder(ko)
    }))

  }

  def interiorAngleGen(min: Int, max: Int): Gen[Double] =
    for
      sides <- Gen.choose(min, max)
    yield interiorAngle(sides)

  val twoBigInteriors: Gen[List[Double]] =
    for
      first  <- interiorAngleGen(100, 1000)
      second <- interiorAngleGen(100, 1000)
    yield List(first, second)

  "A full vertex" must "be composed by at least 3 regular polygons" in {
    check(forAll(twoBigInteriors)(TAU.toDouble ~!=&> _.sum))
  }

  val sevenSmallInteriors: Gen[List[Double]] =
    for
      first   <- interiorAngleGen(3, 6)
      second  <- interiorAngleGen(3, 6)
      third   <- interiorAngleGen(3, 6)
      fourth  <- interiorAngleGen(3, 6)
      fifth   <- interiorAngleGen(3, 6)
      sixth   <- interiorAngleGen(3, 6)
      seventh <- interiorAngleGen(3, 6)
    yield List(first, second, third, fourth, fifth, sixth, seventh)

  it must "be composed by no more than 6 regular polygons" in {
    check(forAll(sevenSmallInteriors)(TAU.toDouble <&~!= _.sum))
  }

  it must "be composed by no more than 3 different regular polygons" in {
    assert(List(3, 4, 5, 6).map(interiorAngle).sum ~!=&> TAU.toDouble)
  }

}
