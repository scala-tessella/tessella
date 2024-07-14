package io.github.scala_tessella.tessella
package creation

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class Uni4HexSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A tiling with a [(3₆);(3₄.6);(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3366_666(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a different [(3₆);(3₄.6);(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3366_666_alt(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a third different [(3₆);(3₄.6);(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3366_666_alt2(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₂.6₂);2x(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂);2x(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366_2x666(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a different [(3₆);(3₂.6₂);2x(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂);2x(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366_2x666_alt(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a third different [(3₆);(3₂.6₂);2x(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂);2x(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366_2x666_alt2(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);2x(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);2x(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_2x3366_666(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a different [(3₆);2x(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);2x(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_2x3366_666_alt(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a fourth different [(3₆);(3₂.6₂);2x(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂);2x(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366_2x666_alt3(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);2x(3₄.6);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[(3₆);2x(3₄.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_2x33336_3366(6, 6).unsafe) shouldBe
      true
  }

}
