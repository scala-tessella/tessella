package io.github.scala_tessella.tessella
package creation

import Outliers.*
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GrowthSemiregularSpec extends AnyFlatSpec with should.Matchers {

  "A (3₄.6) pattern grown quadratically" must "have gonality 1" in {
    p33336.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    p33336.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 0" in {
    p33336.countSymmetries shouldBe
      0
  }

  it can "have rotational symmetry 6" in {
    p33336.countRotationalSymmetries shouldBe
      6
  }

  "A (3₃.4₂) pattern grown quadratically" must "have gonality 1" in {
    p33444.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    p33444.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 2" in {
    p33444.countSymmetries shouldBe
      2
  }

  it can "have rotational symmetry 2" in {
    p33444.countRotationalSymmetries shouldBe
      2
  }

  "A (3.4.6.4) pattern grown quadratically" must "have gonality 1" in {
    p3464.gonality shouldBe
      1
  }

  it must "have hedrality 3" in {
    p3464.hedrality shouldBe
      3
  }

  it can "have reflectional symmetry 6" in {
    p3464.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p3464.countRotationalSymmetries shouldBe
      6
  }

  "A (3.6.3.6) pattern grown quadratically" must "have gonality 1" in {
    p3636.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    p3636.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 6" in {
    p3636.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p3636.countRotationalSymmetries shouldBe
      6
  }

  "A (3.12₂) pattern grown quadratically" must "have gonality 1" in {
    p31212.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    p31212.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 6" in {
    p31212.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p31212.countRotationalSymmetries shouldBe
      6
  }

  "A (4.6.12) pattern grown quadratically" must "have gonality 1" in {
    p4612.gonality shouldBe
      1
  }

  it must "have hedrality 3" in {
    p4612.hedrality shouldBe
      3
  }

  it can "have reflectional symmetry 6" in {
    p4612.countSymmetries shouldBe
      6
  }

  it can "have rotational symmetry 6" in {
    p4612.countRotationalSymmetries shouldBe
      6
  }

  "A (4.8₂) pattern grown quadratically" must "have gonality 1" in {
    p488.gonality shouldBe
      1
  }

  it must "have hedrality 2" in {
    p488.hedrality shouldBe
      2
  }

  it can "have reflectional symmetry 4" in {
    p488.countSymmetries shouldBe
      4
  }

  it can "have rotational symmetry 4" in {
    p488.countRotationalSymmetries shouldBe
      4
  }

}
