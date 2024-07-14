package io.github.scala_tessella.tessella

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class PatternSpec extends AnyFlatSpec with Helper with should.Matchers {

  val pattern: Pattern =
    Pattern(List(
      FullVertex.s("(4.3.4.3.3)"),
      FullVertex.s("(4.4.4.4)"),
      FullVertex.s("(3.3.4.3.4)")
    ))

  "A pattern" can "be printed" in {
    pattern.toString shouldBe
      "[2x(3₂.4.3.4);(4₄)]"
  }

  "A different pattern" can "also be printed" in {
    Pattern.s("[(3₆);(3₄.6)]").toString shouldBe
      "[(3₆);(3₄.6)]"
  }

  it must "be created with sorted vertices" in {
    pattern.vertices.last.toString shouldBe
      "(4₄)"
  }

  it must "be created with polygons sorted in each vertex" in {
    pattern.vertices.head.vertex.toPolygons.map(_.toString) shouldBe
      List("3", "3", "4", "3", "4")
  }

  it can "be created from one FullVertex" in {
    Pattern(FullVertex.s("(4.3.4.3.3)")).toString shouldBe
      "[(3₂.4.3.4)]"
  }

  it can "be printed as a compilable string" in {
    pattern.toCompilableString shouldBe
      """Pattern.s("[2x(3₂.4.3.4);(4₄)]")"""
  }

  it must "have a gonality" in {
    pattern.gonality shouldBe
      2
  }

  it must "have an hedrality" in {
    pattern.hedrality shouldBe
      2
  }

  it must "have an uniformity" in {
    pattern.uniformity shouldBe
      3
  }

  val sp: String =
    "[2x(3₂.4.3.4);(4₄)]"
  val maybePattern: Either[String, Pattern] =
    Pattern.maybe(sp)

  it can "be created from a string with validation" in {
    maybePattern.toOption.get.toString shouldBe
      sp
  }

  it can "be unsafely created from a string without validation" in {
    Pattern.s(sp).toString shouldBe
      sp
  }

  it can "NOT be created from a malformed string with an invalid full vertex" in {
    Pattern.maybe("[2x(3₂.4);(4₄)]") shouldBe
      Left("malformed pattern")
  }

  it can "NOT be created from a malformed string with an unknown 'z' char " in {
    Pattern.maybe("[z2x(3₂.4.3.4);(4₄)]") shouldBe
      Left("malformed pattern")
  }

  it can "NOT be created from a malformed string with an unknown 'q' char" in {
    Pattern.maybe("[2x(3₂.4.3.4);(q)]") shouldBe
      Left("malformed pattern")
  }
  
  "A pattern" can "be checked for having the same polygons of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSamePolygonsOf(Tiling.pattern_2x33344_33434(6, 6).unsafe) shouldBe
      true
  }
  
  it can "be checked for NOT having the same polygons of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSamePolygonsOf(Tiling.pattern_333333(6, 6).unsafe) shouldBe
      false
  }

  it can "be checked for having all the polygons of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").containsAllPolygonsOf(Tiling.pattern_333333(6, 6).unsafe) shouldBe
      true
  }

  "A pattern" can "be checked for having the same vertex configurations of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSameFullVerticesOf(Tiling.pattern_2x33344_33434(6, 6).unsafe) shouldBe
      true
  }

  it can "be checked for NOT having the same vertex configurations of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSameFullVerticesOf(Tiling.pattern_333333(6, 6).unsafe) shouldBe
      false
  }

  it can "be checked for having all the vertex configurations of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").containsAllFullVerticesOf(Tiling.pattern_33434(6, 6).unsafe) shouldBe
      true
  }

  "A pattern" can "be checked for having the same symmetry classes of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSameSymmetryClassesOf(Tiling.pattern_2x33344_33434(6, 6).unsafe) shouldBe
      true
  }

  it can "be checked for NOT having the same symmetry classes of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").hasSameSymmetryClassesOf(Tiling.pattern_333333(6, 6).unsafe) shouldBe
      false
  }

  it can "be checked for having all the symmetry classes of a tiling" in {
    Pattern.s("[2x(3₃.4₂);(3₂.4.3.4)]").containsAllSymmetryClassesOf(Tiling.pattern_33434(6, 6).unsafe) shouldBe
      true
  }

  it can "be checked for NOT having all the symmetry classes of a tiling" in {
    Pattern.s("[(3.4₂.6);(3.6.3.6)]").containsAllSymmetryClassesOf(Tiling.pattern_3446_4x3636(10, 10).unsafe) shouldBe
      false
  }

}
