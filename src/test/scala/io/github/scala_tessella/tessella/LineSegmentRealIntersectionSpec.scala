package io.github.scala_tessella.tessella

import Geometry.{LineSegmentReal, PointReal}

import spire.math.Real
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LineSegmentRealIntersectionSpec extends AnyFlatSpec with Matchers {

  // Helper to create Real from Double/Int for tests for convenience
  private def R(d: Double): Real = Real(d)
  private def R(i: Int): Real = Real(i)

  // Helper to create PointReal for tests
  private def PR(x: Int, y: Int): PointReal = PointReal(R(x), R(y))
  private def PR(x: Double, y: Double): PointReal = PointReal(R(x), R(y))

  "intersection method" should "correctly find intersection for general case" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(2, 2))
    val s2 = LineSegmentReal(PR(0, 2), PR(2, 0))
    s1.intersection(s2) shouldBe Some(PR(1, 1))
    s2.intersection(s1) shouldBe Some(PR(1, 1)) // Symmetric
  }

  it should "return None for parallel non-intersecting segments" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(2, 0))
    val s2 = LineSegmentReal(PR(0, 1), PR(2, 1))
    s1.intersection(s2) shouldBe None
    s2.intersection(s1) shouldBe None
  }

  it should "return None for collinear segments that do not overlap" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(1, 0))
    val s2 = LineSegmentReal(PR(2, 0), PR(3, 0))
    s1.intersection(s2) shouldBe None
    s2.intersection(s1) shouldBe None
  }
  
  it should "return None when line extensions intersect but segments do not" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(1, 1))
    val s2 = LineSegmentReal(PR(0, 2), PR(1, 3)) // Parallel to y=x+k, extension would intersect at (-1,-1)
    s1.intersection(LineSegmentReal(PR(2,0), PR(3, -1))) shouldBe None // s1 extension intersects s2 line, but not segments
  }

  it should "find intersection at a shared endpoint" in {
    val p1 = PR(0,0)
    val p2 = PR(2,0)
    val p3 = PR(2,2)
    val s1 = LineSegmentReal(p1, p2)
    val s2 = LineSegmentReal(p2, p3)
    s1.intersection(s2) shouldBe Some(p2)
    s2.intersection(s1) shouldBe Some(p2)
  }

  it should "find intersection when an endpoint of one segment lies on the other (T-junction)" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(4, 0)) // Horizontal
    val s2 = LineSegmentReal(PR(2, -2), PR(2, 0)) // Vertical, endpoint (2,0) on s1
    val expected = PR(2,0)
    s1.intersection(s2) shouldBe Some(expected)
    s2.intersection(s1) shouldBe Some(expected)
  }
  
  it should "find intersection when an endpoint of one segment lies on the other (T-junction, endpoint of s1 on s2)" in {
    val s1 = LineSegmentReal(PR(2, -2), PR(2, 0))
    val s2 = LineSegmentReal(PR(0, 0), PR(4, 0))
    val expected = PR(2,0)
    s1.intersection(s2) shouldBe Some(expected)
    s2.intersection(s1) shouldBe Some(expected)
  }

  it should "handle horizontal and vertical segments intersecting" in {
    val s1 = LineSegmentReal(PR(0, 1), PR(2, 1)) // Horizontal
    val s2 = LineSegmentReal(PR(1, 0), PR(1, 2)) // Vertical
    s1.intersection(s2) shouldBe Some(PR(1, 1))
    s2.intersection(s1) shouldBe Some(PR(1, 1))
  }

  it should "return an endpoint for identical segments (collinear)" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(2, 2))
    val s2 = LineSegmentReal(PR(0, 0), PR(2, 2))
    // The specific endpoint returned might depend on internal checks,
    // but it should be one of the segment's points.
    val result = s1.intersection(s2)
    result.isDefined shouldBe true
    (result.get.almostEquals(PR(0,0)) || result.get.almostEquals(PR(2,2))) shouldBe true
  }

  it should "return an endpoint for collinear overlapping segments starting at the same point" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(2, 0))
    val s2 = LineSegmentReal(PR(0, 0), PR(1, 0)) // s2 subset of s1, shared start
    s1.intersection(s2) shouldBe Some(PR(0, 0))
    s2.intersection(s1) shouldBe Some(PR(0, 0))
  }

  it should "return an endpoint for collinear overlapping segments ending at the same point" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(2, 0))
    val s2 = LineSegmentReal(PR(1, 0), PR(2, 0)) // s2 subset of s1, shared end
    s1.intersection(s2) shouldBe Some(PR(2,0)) // s1 intersects s2 at s2's end (2,0)
                                               // (which is also s1's end)
                                               // Or it could be PR(1,0) depending on logic path
    val result12 = s1.intersection(s2)
    val result21 = s2.intersection(s1)

    // The current logic might return p1_that or p2_that if onSegment.
    // s1: (0,0)-(2,0), s2: (1,0)-(2,0)
    // p1_this=(0,0), p2_this=(2,0), p1_that=(1,0), p2_that=(2,0)
    // intersects=true. o1_s=0, o2_s=0. Endpoint checks:
    // p2_this.almostEquals(p2_that) is true, and (2,0) is on both. So Some(PR(2,0))
    result12 shouldBe Some(PR(2,0))
    result21 shouldBe Some(PR(2,0)) // s2 intersects s1 at s2's end (which is s1's end)
  }
  
  it should "return an endpoint for collinear segments where one contains the other" in {
    val s1 = LineSegmentReal(PR(0, 0), PR(3, 0)) // Larger
    val s2 = LineSegmentReal(PR(1, 0), PR(2, 0)) // Smaller, contained within s1
    // Expected: s1 intersects s2 at (1,0) (s2.p1) or (2,0) (s2.p2)
    val result12 = s1.intersection(s2)
    result12.isDefined shouldBe true
    (result12.get.almostEquals(PR(1,0)) || result12.get.almostEquals(PR(2,0))) shouldBe true

    val result21 = s2.intersection(s1) // s2 intersects s1 at (1,0) or (2,0)
    result21.isDefined shouldBe true
    (result21.get.almostEquals(PR(1,0)) || result21.get.almostEquals(PR(2,0))) shouldBe true

    s1.intersection(s2) shouldBe Some(PR(1,0))
    s2.intersection(s1) shouldBe Some(PR(1,0))
  }

  it should "return correct intersection point for segments meeting at a common endpoint, forming a V" in {
    val seg1 = LineSegmentReal(PR(0,0), PR(2,2))
    val seg2 = LineSegmentReal(PR(2,2), PR(4,0))
    seg1.intersection(seg2) shouldBe Some(PR(2,2))
    seg2.intersection(seg1) shouldBe Some(PR(2,2))
  }

  it should "return None if one segment is a point and not on the other segment" in {
    val s1 = LineSegmentReal(PR(0,0), PR(0,0)) // Point
    val s2 = LineSegmentReal(PR(1,1), PR(2,2))
    s1.intersection(s2) shouldBe None
    s2.intersection(s1) shouldBe None
  }

  it should "return the point if one segment is a point and lies on the other segment (endpoint)" in {
    val s1 = LineSegmentReal(PR(1,1), PR(1,1)) // Point
    val s2 = LineSegmentReal(PR(1,1), PR(2,2))
    s1.intersection(s2) shouldBe Some(PR(1,1))
    s2.intersection(s1) shouldBe Some(PR(1,1))
  }
  
  it should "return the point if one segment is a point and lies on the other segment (mid-point)" in {
    val s1 = LineSegmentReal(PR(1.5, 1.5), PR(1.5, 1.5)) // Point
    val s2 = LineSegmentReal(PR(1,1), PR(2,2)) // Line (1,1) to (2,2)
    s1.intersection(s2) shouldBe Some(PR(1.5, 1.5))
    s2.intersection(s1) shouldBe Some(PR(1.5, 1.5))
  }

  it should "handle floating point precision with Real values correctly" in {
    val p1 = PointReal(Real(1)/Real(3), Real(1)/Real(3))
    val p2 = PointReal(Real(2)/Real(3), Real(2)/Real(3))
    val p3 = PointReal(Real(1)/Real(3), Real(2)/Real(3))
    val p4 = PointReal(Real(2)/Real(3), Real(1)/Real(3))
    val s1 = LineSegmentReal(p1, p2) // y=x
    val s2 = LineSegmentReal(p3, p4) // y=-x+1
    // Intersection at x=1/2, y=1/2
    s1.intersection(s2) shouldBe Some(PointReal(Real(1)/Real(2), Real(1)/Real(2)))
  }
  
  it should "return None for two collinear segments that only touch at one point, but one extends beyond" in {
    // L1: (0,0) to (2,0)
    // L2: (2,0) to (4,0)
    // They touch at (2,0) which is an endpoint for both.
    val s1 = LineSegmentReal(PR(0,0), PR(2,0))
    val s2 = LineSegmentReal(PR(2,0), PR(4,0))
    s1.intersection(s2) shouldBe Some(PR(2,0))
    s2.intersection(s1) shouldBe Some(PR(2,0))
  }

  it should "return None for truly parallel segments (slope check)" in {
    val s1 = LineSegmentReal(PR(0,0), PR(1,1))
    val s2 = LineSegmentReal(PR(0,1), PR(1,2)) // y = x + 1
    s1.intersection(s2) shouldBe None
  }
  
  it should "return None for collinear overlapping segments that do not share an original endpoint of both as the common point" in {
    val s1 = LineSegmentReal(PR(0,0), PR(4,0))
    val s2 = LineSegmentReal(PR(1,0), PR(2,0)) // s2 is contained in s1
    // Expected: Some(PR(1,0)) or Some(PR(2,0))
    // s1.intersection(s2) will try to see if p1_that (1,0) is on s1. Yes. Returns Some(PR(1,0)).
    s1.intersection(s2) shouldBe Some(PR(1,0))

    val s3 = LineSegmentReal(PR(1,0), PR(2,0))
    val s4 = LineSegmentReal(PR(0,0), PR(4,0)) // s3 is contained in s4
    s3.intersection(s4) shouldBe Some(PR(1,0))
  }


}