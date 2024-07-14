package io.github.scala_tessella.tessella
package creation

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class ReticulateSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A uniform tiling as a holed triangular net" can "NOT be generated with width 4" in {
    Tiling.pattern_3636_other(4, 3) shouldEqual
      Left("Width should be even and greater than 4")
  }

  it can "NOT be generated with height 1" in {
    Tiling.pattern_3636_other(6, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  it can "be generated" in {
    Tiling.pattern_3636_other(6, 2).isRight shouldBe
      true
  }

  "Another uniform tiling" can "NOT be generated with width 2" in {
    Tiling.pattern_33336(2, 1) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "be generated" in {
    Tiling.pattern_33336(4, 1).isRight shouldBe
      true
  }

  "A tiling with a [(3₆);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₄.6)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336(4, 4).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₂.6₂)] pattern" can "NOT be generated with width 2" in {
    Tiling.pattern_33336_3366(2, 2) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "NOT be generated with height 1" in {
    Tiling.pattern_33336_3366(4, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  "A tiling with a [(3.6.3.6);(3₂.6₂)] pattern" can "NOT be generated with width 2" in {
    Tiling.pattern_3636_3366(2, 2) shouldEqual
      Left("Width should be even and greater than 2")
  }

  it can "NOT be generated with height 1" in {
    Tiling.pattern_3636_3366(4, 1) shouldEqual
      Left("Height should be greater than 1")
  }

  it can "be generated" in {
    Pattern.s("[(3.6.3.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_3636_3366(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₂.6₂);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₂.6₂);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_3366_666(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₄.6);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3366(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a different [(3₆);(3₄.6);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3366_alt(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₂.6₂);(3.6.3.6);(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₂.6₂);(3.6.3.6);(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_3366_3636_666_alt(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);(3₄.6);(3.6.3.6)] pattern" can "be created" in {
    Pattern.s("[(3₆);(3₄.6);(3.6.3.6)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_33336_3636(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [2x(3₆);(3₄.6)] pattern" can "be created" in {
    Pattern.s("[2x(3₆);(3₄.6)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_2x333333_33336(8, 8).unsafe) shouldBe
      true
  }

  "A tiling with a [2x(3₆);(3₄.6);(3₂.6₂)] pattern" can "be created" in {
    Pattern.s("[2x(3₆);(3₄.6);(3₂.6₂)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_2x333333_33336_3366(10, 10).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₆);2x(3₂.6₂);4x(6₃)] pattern" can "be created" in {
    Pattern.s("[(3₆);2x(3₂.6₂);4x(6₃)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_333333_2x3366_4x666(10, 10).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₂.4.3.4)] pattern" can "be created" in {
    Pattern.s("[(3₂.4.3.4)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_33434(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₃.4₂);(3₂.4.3.4)] pattern" can "be created" in {
    Pattern.s("[(3₃.4₂);(3₂.4.3.4)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_33344_33434(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a [2x(3₃.4₂);(3₂.4.3.4)] pattern" can "be created" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_2x33344_33434(6, 6).unsafe) shouldBe
      true
  }

  "A tiling with a [(3₃.4₂);(3₂.4.3.4);(4₄)] pattern" can "be created" in {
    Pattern.s("[(3₃.4₂);(3₂.4.3.4);(4₄)]")
      .hasSameSymmetryClassesOf(Tiling.pattern_33344_33434_4444(6, 6).unsafe) shouldBe
      true
  }

}
