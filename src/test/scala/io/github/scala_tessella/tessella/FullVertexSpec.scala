package io.github.scala_tessella.tessella

import RegularPolygon.{Polygon, Vertex}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class FullVertexSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A valid full vertex" can "NOT be publicly created with a constructor" in {
    "FullVertex(Vector(Polygon(6), Polygon(6), Polygon(6)))" shouldNot compile
  }

  it can "be publicly created through validation" in {
    "FullVertex.maybe(Vector(Polygon(6), Polygon(6), Polygon(6)))" should compile
  }

  it can "be publicly created through validation as an Either" in {
    FullVertex.maybe(Vector(Polygon(6), Polygon(6), Polygon(6))).isRight shouldBe
      true
  }

  "An invalid less than full vertex" must "fail as an Either" in {
    FullVertex.maybe(Vector(Polygon(6), Polygon(6))) shouldBe
      Left(FullVertex.notFullErrMsg(Vector(Polygon(6), Polygon(6)).alphaSum))
  }

  "An invalid more than full vertex" must "fail as an Either" in {
    FullVertex.maybe(Vector(Polygon(6), Polygon(6), Polygon(60))) shouldBe
      Left(FullVertex.notFullErrMsg(Vector(Polygon(6), Polygon(6), Polygon(60)).alphaSum))
  }

  "A valid 6.6.6 full vertex" can "be created as an Either from a string" in {
    FullVertex.maybe("(6.6.6)").isRight shouldBe
      true
  }

  it can "be created as an Either from a different string" in {
    FullVertex.maybe("(6*3)").isRight shouldBe
      true
  }

  it can "be created as an Either from a third different string" in {
    FullVertex.maybe("(6₃)").isRight shouldBe
      true
  }

  it can "NOT be created from a string if opening parenthesis is missing" in {
    FullVertex.maybe("6₃)") shouldBe
      Left("Must start with (")
  }

  it can "NOT be created from a string if closing parenthesis is missing" in {
    FullVertex.maybe("(6₃") shouldBe
      Left("Must end with )")
  }

  it can "NOT be created from a string containing an unknown symbol" in {
    FullVertex.maybe("(6?)") shouldBe
      Left("Unknown symbol")
  }

  it can "be created as an Either from polygon sizes" in {
    FullVertex.maybe(6, 6, 6).isRight shouldBe
      true
  }

  it can "be unsafely created from strings" in {
    FullVertex.s("(6*3)").vertex shouldBe
      Vertex(6, 6, 6)
  }

  it can "be unsafely created from sizes" in {
    FullVertex.p(6, 6, 6).vertex shouldBe
      Vertex(6, 6, 6)
  }

  "An invalid 6.6.2 vertex" can "NOT be created as an Either from polygon sizes" in {
    FullVertex.maybe(2, 6, 6) shouldBe
      Left("Invalid number of sides")
  }

  it can "NOT be unsafely created from strings" in {
    val caught = intercept[java.util.NoSuchElementException] {
      FullVertex.s("(6*2)")
    }
    caught.getMessage shouldBe "None.get"
  }

  it can "NOT be unsafely created from sizes" in {
    val caught = intercept[java.util.NoSuchElementException] {
      FullVertex.p(6, 6, 2)
    }
    caught.getMessage shouldBe "None.get"
  }

  "A valid full vertex" can "be printed as a compilable string" in {
    FullVertex.p(6, 6, 6).toCompilableString shouldBe
      """FullVertex.s("(6₃)")"""
  }

  it can "be converted to a Pattern" in {
    Pattern(FullVertex.p(6, 6, 6)).toString shouldBe
      "[(6₃)]"
  }

  it can "be compared to itself" in {
    FullVertex.s("(3.3.4.3.4)").compare(FullVertex.s("(3.3.4.3.4)")) shouldBe
      0
  }

  it can "be compared to a different other" in {
    FullVertex.s("(3.3.4.3.4)").compare(FullVertex.s("(4.3.4.6)")) shouldBe
      -1
  }

  it can "be compared to a different other with the same polygons" in {
    FullVertex.s("(3.3.4.3.4)").compare(FullVertex.s("(3.4.3.4.3)")) shouldBe
      -1
  }

  it can "be equal to a self rotation" in {
    FullVertex.s("(3.3.4.3.4)") shouldBe FullVertex.s("(3.4.3.4.3)")
  }

}
