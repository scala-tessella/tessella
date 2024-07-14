package io.github.scala_tessella.tessella
package creation

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class UniTriangleSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A tiling with a [(3₆);(3₄.6)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_alt(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₂.6₂);(3.6.3.6);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₂.6₂);(3.6.3.6);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_3366_3636_666(10, 10).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₄.6);(3.6.3.6)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3.6.3.6)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3636(10, 10).unsafe) shouldBe
      true
  }

  "A tiling with a [2x(3₆);(3₄.6);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[2x(3₆);(3₄.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_2x333333_33336_3366(10, 10).unsafe) shouldBe
      true
  }

}
