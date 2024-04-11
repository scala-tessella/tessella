package io.github.scala_tessella.tessella

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AccuracySpec extends AnyFlatSpec with Accuracy with should.Matchers {

  "Math.PI" must "have a given value" in {
    Math.PI shouldBe
      3.141592653589793
  }

  val almostPI: Double =
    3.1415926536
  val lessThanPi: Double =
    3.141592653
  val moreThanPi: Double =
    3.141592654

  it must "NOT be equal to another double out of standard accuracy" in {
    Math.PI should not equal
      almostPI
  }

  it must "be equal to another double within standard accuracy" in {
    (Math.PI ~= almostPI) shouldBe
      true
  }

  it must "NOT be equal to another double if lesser and out of standard accuracy" in {
    (Math.PI ~= lessThanPi) shouldBe
      false
  }

  it must "NOT be equal to another double if greater and out of standard accuracy" in {
    (Math.PI ~= moreThanPi) shouldBe
      false
  }

  val GIVEN_ACCURACY: Double =
    1e-1

  "1.0" must "be approximately equal to another lesser double within a given accuracy" in {
    (1.0 ~=(0.9, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "NOT be approximately equal to another lesser double out of a given accuracy" in {
    (1.0 ~= (0.8999999, GIVEN_ACCURACY)) shouldBe
      false
  }

  it must "NOT be approximately equal to another greater double within a given accuracy" in {
    (1.0 ~= (1.0999999, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "NOT be approximately equal to another greater double out of a given accuracy" in {
    (1.0 ~= (1.1, GIVEN_ACCURACY)) shouldBe
      false
  }

  it must "be approximately equal or lesser than another lesser double within a given accuracy" in {
    (1.0 <|~=(0.9, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "NOT be equal or lesser than another lesser double" in {
    (1.0 <= 0.9) shouldBe
      false
  }

  it must "NOT be approximately equal or lesser than another lesser double out of a given accuracy" in {
    (1.0 <|~= (0.8999999, GIVEN_ACCURACY)) shouldBe
      false
  }

  it must "be approximately equal or greater than another greater double within a given accuracy" in {
    (1.0 ~=|> (1.0999999, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "NOT be equal or greater than another greater double" in {
    (1.0 >= 1.0999999) shouldBe
      false
  }

  it must "NOT be approximately equal or greater than another greater lesser double out of a given accuracy" in {
    (1.0 ~=|> (1.1, GIVEN_ACCURACY)) shouldBe
      false
  }

  it must "be just lesser than another double and not approximately equal to within a given accuracy" in {
    (1.0 <&~!= (1.1, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "be just lesser than another double" in {
    (1.0 < 1.1) shouldBe
      true
  }

  it must "NOT be just lesser than another double and not approximately equal out of a given accuracy" in {
    (1.0 <&~!= (1.0999999, GIVEN_ACCURACY)) shouldBe
      false
  }

  it must "be just greater than another double and not approximately equal to within a given accuracy" in {
    (1.0 ~!=&> (0.8999999, GIVEN_ACCURACY)) shouldBe
      true
  }

  it must "be just greater than another double" in {
    (1.0 > 0.8999999) shouldBe
      true
  }

  it must "NOT be just greater than another double and not approximately equal out of a given accuracy" in {
    (1.0 ~!=&> (0.9, GIVEN_ACCURACY)) shouldBe
      false
  }

  "Math.PI" can "be rounded to Long with standard accuracy" in {
    Math.PI.rounded() shouldBe
      3141593
  }

  it can "be rounded to Long with given accuracy" in {
    Math.PI.rounded(1e2) shouldBe
      314
  }

}
