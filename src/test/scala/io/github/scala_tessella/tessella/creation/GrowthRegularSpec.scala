package io.github.scala_tessella.tessella
package creation

import Outliers.{hexHexOfSide3, sqr3x3Growth, triHexOfSide3}
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GrowthRegularSpec extends AnyFlatSpec with should.Matchers {

  "A (3⁶) pattern grown quadratically" must "have gonality 1" in {
    triHexOfSide3.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    triHexOfSide3.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 6" in {
    triHexOfSide3.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    triHexOfSide3.countRotationalSymmetries shouldBe
      6
  }

  "A (4⁴) pattern grown quadratically" must "have gonality 1" in {
    sqr3x3Growth.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    sqr3x3Growth.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 4" in {
    sqr3x3Growth.countSymmetries shouldBe
      4
  }

  it can "have rotational symmetry 4" in {
    sqr3x3Growth.countRotationalSymmetries shouldBe
      4
  }

  "A (6³) pattern grown quadratically" must "have gonality 1" in {
    hexHexOfSide3.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    hexHexOfSide3.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 6" in {
    hexHexOfSide3.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    hexHexOfSide3.countRotationalSymmetries shouldBe
      6
  }

}
