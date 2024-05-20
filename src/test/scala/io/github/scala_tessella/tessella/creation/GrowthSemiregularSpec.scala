package io.github.scala_tessella.tessella
package creation

import Outliers.*
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GrowthSemiregularSpec extends AnyFlatSpec with should.Matchers {

  "A (3₄.6) pattern grown quadratically" must "have gonality 1" in {
    triHexHexoid.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    triHexHexoid.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 0" in {
    triHexHexoid.countSymmetries shouldBe
      0
  }

  it can "have rotational symmetry 6" in {
    triHexHexoid.countRotationalSymmetries shouldBe
      6
  }

  "A (3₃.4₂) pattern grown quadratically" must "have gonality 1" in {
    triSqrSquaroid.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    triSqrSquaroid.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 2" in {
    triSqrSquaroid.countSymmetries shouldBe
      2
  }

  it can "have rotational symmetry 2" in {
    triSqrSquaroid.countRotationalSymmetries shouldBe
      2
  }

  "A (3.4.6.4) pattern grown quadratically" must "have gonality 1" in {
    triSqrHexHexoid.gonality shouldBe
      1
  }

  it must "have hedrality 3" in {
    triSqrHexHexoid.hedrality shouldBe
      3
  }

  it can "have reflectional symmetry 6" in {
    triSqrHexHexoid.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    triSqrHexHexoid.countRotationalSymmetries shouldBe
      6
  }

  "A (3.6.3.6) pattern grown quadratically" must "have gonality 1" in {
    triHexHex.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    triHexHex.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 6" in {
    triHexHex.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    triHexHex.countRotationalSymmetries shouldBe
      6
  }

  "A (3.12₂) pattern grown quadratically" must "have gonality 1" in {
    triDodHexoid.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    triDodHexoid.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 6" in {
    triDodHexoid.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    triDodHexoid.countRotationalSymmetries shouldBe
      6
  }

  "A (4.6.12) pattern grown quadratically" must "have gonality 1" in {
    sqrHexDodHexoid.gonality shouldBe
      1
  }

  it must "have hedrality 3" in {
    sqrHexDodHexoid.hedrality shouldBe
      3
  }

  it can "have reflectional symmetry 6" in {
    sqrHexDodHexoid.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    sqrHexDodHexoid.countRotationalSymmetries shouldBe
      6
  }

  "A (4.8₂) pattern grown quadratically" must "have gonality 1" in {
    sqrOctSquaroid.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    sqrOctSquaroid.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 4" in {
    sqrOctSquaroid.countSymmetries shouldBe
      4
  }

  it can "have rotational symmetry 4" in {
    sqrOctSquaroid.countRotationalSymmetries shouldBe
      4
  }

}
