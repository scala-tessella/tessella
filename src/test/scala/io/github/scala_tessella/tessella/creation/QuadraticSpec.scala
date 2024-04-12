package io.github.scala_tessella.tessella
package creation

import TilingGrowth.growFullVertex
import TilingGrowth.OtherNodeStrategy.*
import TilingGrowth.PerimeterStrategy.*
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class QuadraticSpec extends AnyFlatSpec with Helper with Quadratic with should.Matchers {

  "A hexagon of triangles grown quadratically" can "have side 3" in {
    Tiling.triangularHex(3).isRight shouldBe
      true
  }

  it can "NOT have side 0" in {
    Tiling.triangularHex(0) shouldEqual
      Left("Side should be greater than 0")
  }

  "A full vertex 333333" can "be grown" in {
    val grown: Tiling =
      growFullVertex(54, FullVertex.p(3, 3, 3, 3, 3, 3), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 4444" can "be grown" in {
    val grown: Tiling =
      growFullVertex(64, FullVertex.p(4, 4, 4, 4), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      4
  }

  "A full vertex 666" can "be grown" in {
    val grown: Tiling =
      growFullVertex(61, FullVertex.p(6, 6, 6), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 33336" can "be grown" in {
    val grown: Tiling =
      growFullVertex(
        61,
        FullVertex.p(3, 3, 3, 3, 6),
        (List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(NARROWER_ANGLE, LOWER_ORDINAL), false)
      ).unsafe
    grown.countRotationalSymmetries shouldBe
      6
  }

  "A full vertex 33344" can "be grown" in {
    val grown: Tiling =
      growFullVertex(
        38,
        FullVertex.p(3, 3, 3, 4, 4),
        (List(LOWEST_ORDINAL), List(BEFORE_PERIMETER), true)
      ).unsafe
    grown.countSymmetries shouldBe
      2
  }

  "A full vertex 33434" can "be grown" in {
    val grown: Tiling =
      growFullVertex(81, FullVertex.p(3, 3, 4, 3, 4), standardStrategyFromSmaller).unsafe
    grown.countRotationalSymmetries shouldBe
      4
  }

  "A full vertex 3464" can "be grown" in {
    val grown: Tiling =
      growFullVertex(85, FullVertex.p(3, 4, 6, 4), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 3636" can "be grown" in {
    val grown: Tiling =
      growFullVertex(55, FullVertex.p(3, 6, 3, 6), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 31212" can "be grown" in {
    val grown: Tiling =
      growFullVertex(55, FullVertex.p(3, 12, 12), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 4612" can "be grown" in {
    val grown: Tiling =
      growFullVertex(61, FullVertex.p(4, 6, 12), standardStrategy).unsafe
    grown.countSymmetries shouldBe
      6
  }

  "A full vertex 488" can "be grown" in {
    val grown: Tiling =
      growFullVertex(49, FullVertex.p(4, 8, 8), standardStrategyFromSmaller).unsafe
    grown.countSymmetries shouldBe
      4
  }

}
