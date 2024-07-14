package io.github.scala_tessella.tessella
package creation

import Outliers.{p666_grown_hexagon, p4444_3by3_grown, p333333_grown_hexagon}
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GrowthRegularSpec extends AnyFlatSpec with should.Matchers {

  "A (3₆) pattern grown quadratically" must "have gonality 1" in {
    p333333_grown_hexagon.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    p333333_grown_hexagon.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 6" in {
    p333333_grown_hexagon.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p333333_grown_hexagon.countRotationalSymmetries shouldBe
      6
  }

  "A (4₄) pattern grown quadratically" must "have gonality 1" in {
    p4444_3by3_grown.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    p4444_3by3_grown.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 4" in {
    p4444_3by3_grown.countSymmetries shouldBe
      4
  }

  it can "have rotational symmetry 4" in {
    p4444_3by3_grown.countRotationalSymmetries shouldBe
      4
  }

  "A (6₃) pattern grown quadratically" must "have gonality 1" in {
    p666_grown_hexagon.gonality shouldBe
      1
  }

  it must "have hedrality 1" in {
    p666_grown_hexagon.hedrality shouldBe
      1
  }

  it can "have reflectional symmetry 6" in {
    p666_grown_hexagon.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p666_grown_hexagon.countRotationalSymmetries shouldBe
      6
  }

}
