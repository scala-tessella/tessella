package io.github.scala_tessella.tessella
package creation

import TilingUniformity.uniformity

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class ReticulateSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A uniform tiling as a holed triangular net" can "NOT be generated with width 4" in {
    Tiling.uniform(4, 3) shouldEqual
      Left("Width should be even and greater than 4")
  }

  it can "NOT be generated with height 1" in {
    Tiling.uniform(6, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  it can "be generated" in {
    Tiling.uniform(6, 2).isRight shouldBe
      true
  }

  "Another uniform tiling" can "NOT be generated with width 2" in {
    Tiling.uniform2(2, 1) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "be generated" in {
    Tiling.uniform2(4, 1).isRight shouldBe
      true
  }

  "A 2 uniform tiling" can "be generated" in {
    Tiling.twoUniform(2, 1).isRight shouldBe
      true
  }

  "A second 2 uniform tiling" can "be generated" in {
    Tiling.twoUniform2(2, 1).isRight shouldBe
      true
  }

  "A third 2 uniform tiling" can "be generated" in {
    Tiling.twoUniform3(2, 1).isRight shouldBe
      true
  }

  "A fourth 2 uniform tiling" can "NOT be generated with width 2" in {
    Tiling.twoUniform4(2, 2) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "NOT be generated with height 1" in {
    Tiling.twoUniform4(4, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  it can "be generated" in {
    Tiling.twoUniform4(4, 2).isRight shouldBe
      true
  }

  "A fifth 2 uniform tiling" can "NOT be generated with width 2" in {
    Tiling.twoUniform5(2, 2) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "NOT be generated with height 1" in {
    Tiling.twoUniform5(4, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  it can "be generated" in {
    Tiling.twoUniform5(4, 2).isRight shouldBe
      true
  }

  "A 3 uniform tiling with gonality 3" can "be generated" in {
    val t: Tiling =
      Tiling.threeUniformOneOneOne(8, 5).unsafe
    (t.uniformity, t.gonality) shouldBe
      (3, 3)
  }

  "A second 3 uniform tiling" can "be generated" in {
    Tiling.threeUniformOneOneOne2(2, 1).isRight shouldBe
      true
  }

  "A third 3 uniform tiling" can "be generated" in {
    Tiling.threeUniformOneOneOne3(2, 1).isRight shouldBe
      true
  }

  "A fourth 3 uniform tiling" can "be generated" in {
    Tiling.threeUniformOneOneOne4(4, 2).isRight shouldBe
      true
  }

  "A fifth 3 uniform tiling" can "be generated" in {
    Tiling.threeUniformOneOneOne5(2, 2).isRight shouldBe
      true
  }

  "A sixth 3 uniform tiling" can "be generated" in {
    Tiling.threeUniformOneOneOne6(2, 2).isRight shouldBe
      true
  }

  "A three uniform tiling with gonality 2" can "be generated" in {
    val t: Tiling =
      Tiling.threeUniformTwoOne(8, 5).unsafe
    (t.uniformity, t.gonality) shouldBe
      (3, 2)
  }

  "A 4 uniform tiling with gonality 3" can "be generated" in {
    val t: Tiling =
      Tiling.fourUniformTwoOneOne8(16, 10).unsafe
    (t.uniformity, t.gonality) shouldBe
      (4, 3)
  }

  "A 7 uniform tiling with gonality 3" can "be generated" in {
    val t: Tiling =
      Tiling.sevenUniformFourTwoOne(16, 10).unsafe
    (t.uniformity, t.gonality) shouldBe
      (7, 3)
  }

}
