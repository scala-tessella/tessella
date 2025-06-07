package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU_4
import TilingCoordinates.*
import Topology.{--, Edge, Node}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GeometrySpec extends AnyFlatSpec with Helper with should.Matchers:

  "A radian" can "be created as an opaque type" in {
    Radian(0).toDouble shouldBe
      0.0
  }

  def foo(radian: Radian) =
    true

  "A function" must "accept a Radian param type" in {
    "foo(Radian(1.0))" should compile
  }

  it must "NOT accept a Radian param type" in {
    "foo(1.0)" shouldNot compile
  }

  "A radian" can "be added to another Radian" in {
    Radian(1.0) + Radian(3.0) shouldEqual
      4.0
  }

  it can "be subtracted to another Radian" in {
    Radian(3.0) - Radian(1.0) shouldEqual
      2.0
  }

  it can "be multiplied by times" in {
    Radian(3.0) * 2 shouldEqual
      6.0
  }

  it can "be divided by times" in {
    Radian(2.0) / 2 shouldEqual
      1.0
  }

  val point: Point =
    Point(1.0, -1.0)

  val almost: Point =
    Point(0.9999999999999998, -1.0)

  "A Point2D" can "be not equal to another one" in {
    point.equals(almost) shouldBe
      false
  }

  it can "be approximately equal to another one at a given level of accuracy" in {
    point.almostEquals(almost, 1e-12) shouldBe
      true
  }

  it can "be NOT approximately equal to another one at a greater level of accuracy" in {
    point.almostEquals(almost, 1e-16) shouldBe
      false
  }

  it can "return a new point moved by polar coords" in {
    point.plusPolar(2.0)(TAU_4).almostEquals(Point(1.0, 1.0), ACCURACY) shouldBe
      true
  }

  it can "return a new point moved by 1 unit in a polar direction" in {
    point.plusPolarUnit(TAU_4) shouldBe
      Point(1.0, 0.0)
  }

  it can "return the angle to another point" in {
    point.angleTo(Point(1.0, 0.0)) shouldBe
      TAU_4
  }

  it can "be aligned to other two points" in {
    point.alignWithStart(Point(), Point(1.0, 0.0)).almostEquals(point, ACCURACY) shouldBe
      true
  }

  it can "be flipped vertically" in {
    point.flipVertically shouldBe
      Point(1.0, 1.0)
  }

  val points: Vector[Point] =
    Vector(point, Point(), Point(3.0, 3.0), almost)

  "A set of Point2D" can "have no duplicates, with approximation" in {
    points.tail.areAllDistinct shouldBe
      true
  }

  it can "have duplicates, with approximation, when an almost equal pont is added" in {
    points.areAllDistinct shouldBe
      false
  }

  it can "return only the duplicates, with approximation" in {
    points.almostEqualCouples.toList shouldBe
      List((point, almost))
  }

  it can "be approximately equal to another set" in {
    points.almostEquals(Vector(almost, Point(), Point(3.0, 3.0), point)) shouldBe
      true
  }

  val segment: LineSegmentReal =
    LineSegmentReal(PointReal(0, 0), PointReal(1, -1))

  val intersecting: LineSegmentReal =
    LineSegmentReal(PointReal(0.0, -1.0), PointReal(1.0, 0.0))

  val sameOrigin: LineSegmentReal =
    LineSegmentReal(PointReal(0, 0), PointReal(3.0, 3.0))

//  it can "return the point of intersection (from original test)" in {
//    segment.intersection(intersecting).get.almostEquals(Point(0.5, -0.5)) shouldBe true
//  }

  it can "lesser intersect with another one (from original test)" in {
    segment.lesserIntersects(intersecting) shouldBe true
  }

  it can "contain a point at edges" in {
    segment.containsAtEdges(PointReal(0, 0)) shouldBe true // segment is (0,0) to (1,-1)
    segment.containsAtEdges(PointReal(1, -1)) shouldBe true // point is (1,-1)
  }

  it can "NOT contain a point at edges" in {
    segment.containsAtEdges(sameOrigin.point2) shouldBe // sameOrigin.point2 is (3,3)
      false
  }

  val nonIntersectingPolygon: SimplePolygonReal =
    SimplePolygonReal(List(
      PointReal(0.0, 0.0),
      PointReal(1.0, 0.0),
      PointReal(1.0, 1.0),
      PointReal(0.0, 1.0)
    ))

  "A SimplePolygon2D" can "be non-self-intersecting" in {
    nonIntersectingPolygon.isSelfIntersecting shouldBe false
    nonIntersectingPolygon.intersectingSides.toList shouldBe empty
  }

  val single: SimplePolygonReal =
    SimplePolygonReal(List(
      PointReal(0, 0),
      PointReal(1.0, 1.0),
      PointReal(1.0, 0.0),
      PointReal(0.0, 1.0)
    ))

  it can "be self intersecting" in {
    single.isSelfIntersecting shouldBe
      true
  }

  it can "return the couples of segments self intersecting" in {
    single.intersectingSides.toList shouldBe
      List(
        (
          LineSegmentReal(PointReal(1.0, 0.0), PointReal(0.0, 1.0)),
          LineSegmentReal(PointReal(0, 0), PointReal(1.0, 1.0))
        )
      )
  }

  val regular: RegularPolygon2D =
    RegularPolygon2D(List(
      Point(),
      Point(0.0, 1.0),
      Point(1.0, 1.0),
      Point(1.0, 0.0)
    ))

  "A regular polygon" must "have a centre" in {
    regular.center() shouldBe
      Point(0.5, 0.5)
  }

//  "A set of LineSegment2D" can "be approximately equal to another set" in {
//    List(segment, sameOrigin)
//      .almostEquals(List(LineSegment(Point(), almost), sameOrigin)) shouldBe
//      true
//  }

  val coords: Coords =
    Map(
      Node(1) -> Point(0.0, -2.0),
      Node(2) -> Point(1.0, -2.0),
      Node(3) -> Point(1.0, -1.0),
      Node(4) -> Point(0.0, -1.0)
    )

  "A set of coordinates" can "be flipped vertically if node 3 is negative on the y-axis" in {
    coords.flipVertically shouldBe
      Map(
        1 -> Point(0.0, 2.0),
        2 -> Point(1.0, 2.0),
        3 -> Point(1.0, 1.0),
        4 -> Point(0.0, 1.0)
      )
  }

  it can "be aligned to the two start points" in {
    coords.alignWithStart.almostEqualsMap(Map(
      Node(1) -> Point(),
      Node(2) -> Point(1.0, 0.0),
      Node(3) -> Point(1.0, 1.0),
      Node(4) -> Point(0.0, 1.0)
    )) shouldBe
      true
  }

  val anEdge: Edge =
    1--2

  "An Edge" can "be converted to a LineSegment2D" in {
    anEdge.toSegment(Map(Node(1) -> Point(), Node(2) -> Point(1.0, 0.0))) shouldBe
      LineSegment(Point(), Point(1.0, 0.0))
  }

  val second: Edge =
    3--1

  "A sequence of edges" can "be converted to a sequence of LineSegment2D" in {
    List(anEdge, second).toSegments(Map(
      Node(1) -> Point(),
      Node(2) -> Point(1.0, 0.0),
      Node(3) -> Point(1.0, 1.0),
    )) shouldEqual
      List(
        LineSegment(Point(), Point(1.0, 0.0)),
        LineSegment(Point(), Point(1.0, 1.0))
      )
  }

  it can "be converted to Box" in {
    List(anEdge, second).toBox(Map(
      Node(1) -> Point(-9.0, 10.0),
      Node(2) -> Point(11.0, 10.0),
      Node(3) -> Point(11.0, 11.0),
    )) shouldEqual
      Box(-9.0, 11.0, 10.0, 11.0)
  }

  val someCoords: Coords =
    Map(
      Node(55) -> Point(0.49999999999960043, -0.866025403784008),
      Node(57) -> Point(-0.3660254037849453, -1.3660254037838226),
      Node(68) -> Point(-0.3660254037847309, -0.36602540378382264)
    )

  "Another box" can "be created" in {
    List(57--68, 55--68).toBox(someCoords) shouldEqual
      Box(-0.3660254037849453, 0.49999999999960043, -1.3660254037838226, -0.36602540378382264)
  }

  "A regular triangle" must "have a center" in {
    RegularPolygon2D(someCoords.values.toList).center() shouldBe
      Point(-0.07735026919002526, -0.8660254037838845)
  }

//  it can "be created incorrect with a deprecated method" in {
//    List(57--68, 55--68).toBoxOld(someCoords) shouldEqual
//      Box2D(-0.3660254037849453, 0.49999999999960043, -1.3660254037838226, 4.9E-324)
//  }

  "Point.ccw" should "return 1 for counter-clockwise points" in {
    val p0 = Point(0, 0)
    val p1 = Point(1, 0)
    val p2 = Point(1, 1)
    Point.ccw(p0, p1, p2) shouldBe 1
  }

  it should "return -1 for clockwise points" in {
    val p0 = Point(0, 0)
    val p1 = Point(1, 0)
    val p2 = Point(1, -1)
    Point.ccw(p0, p1, p2) shouldBe -1
  }

  // Collinear cases based on the specific implementation:
  // Docstring: "0 if the point P2 is located on the line segment [P0 P1]"

  it should "return 0 when p2 is on segment p0-p1 (p0-p2-p1)" in {
    val p0 = Point(0, 0)
    val p1 = Point(2, 0)
    val p2 = Point(1, 0) // p2 between p0 and p1
    Point.ccw(p0, p1, p2) shouldBe 0

    val p2_alt = Point(0, 1) // p0=(0,0), p1=(0,2), p2=(0,1)
    Point.ccw(Point(0, 0), Point(0, 2), p2_alt) shouldBe 0
  }

  it should "return 0 when p2 is p0" in {
    val p0 = Point(0, 0)
    val p1 = Point(2, 0)
    val p2 = Point(0, 0) // p2 equals p0
    Point.ccw(p0, p1, p2) shouldBe 0
  }

  it should "return 0 when p2 is p1" in {
    val p0 = Point(0, 0)
    val p1 = Point(2, 0)
    val p2 = Point(2, 0) // p2 equals p1
    Point.ccw(p0, p1, p2) shouldBe 0
  }

  // Other collinear cases based on implementation details
  it should "return 1 when p0-p1-p2 are collinear and p2 is beyond p1" in {
    val p0 = Point(0, 0)
    val p1 = Point(1, 0)
    val p2 = Point(2, 0) // p2 beyond p1
    Point.ccw(p0, p1, p2) shouldBe 1
  }

  it should "return -1 when p2-p0-p1 are collinear and p2 is behind p0" in {
    val p0 = Point(1, 0)
    val p1 = Point(2, 0)
    val p2 = Point(0, 0) // p2 behind p0
    Point.ccw(p0, p1, p2) shouldBe -1
  }

  it should "return 0 when p0, p1, p2 are the same point" in {
    val p = Point(1, 1)
    Point.ccw(p, p, p) shouldBe 0
  }

  it should "return 1 when p0=p1 and p2 is different from p0" in {
    // dx1=0, dy1=0. Collinear branch. !(0<0)&&!(0<0) is true.
    // hypot(0,0) < hypot(dx2,dy2) => 0 < dist(p0,p2). If p2 != p0, this is true. Returns 1.
    val p0 = Point(0, 0)
    val p1 = Point(0, 0) // p0=p1
    val p2 = Point(1, 1) // p2 is different
    Point.ccw(p0, p1, p2) shouldBe 1
  }

  it should "return 0 when p1=p2 and p0 is different (p2 on segment p0-p1 as p2=p1)" in {
    // p2 coincides with p1, so p2 is on segment [p0,p1] (at the endpoint)
    val p0 = Point(0, 0)
    val p1 = Point(1, 1)
    val p2 = Point(1, 1) // p1=p2
    Point.ccw(p0, p1, p2) shouldBe 0
  }

  it should "return 0 when p0=p2 and p1 is different (p2 on segment p0-p1 as p2=p0)" in {
    // p2 coincides with p0, so p2 is on segment [p0,p1] (at the startpoint)
    val p0 = Point(0, 0)
    val p1 = Point(1, 1)
    val p2 = Point(0, 0) // p0=p2
    Point.ccw(p0, p1, p2) shouldBe 0
  }

  it should "handle vertical line CCW" in {
    val p0 = Point(0, 0)
    val p1 = Point(0, 1)
    val p2 = Point(-1, 1) // to the left
    Point.ccw(p0, p1, p2) shouldBe 1
  }

  it should "handle vertical line CW" in {
    val p0 = Point(0, 0)
    val p1 = Point(0, 1)
    val p2 = Point(1, 1) // to the right
    Point.ccw(p0, p1, p2) shouldBe -1
  }

  it should "handle horizontal line, p2 above" in {
    val p0 = Point(0, 0)
    val p1 = Point(1, 0)
    val p2 = Point(0.5, 1) // above
    Point.ccw(p0, p1, p2) shouldBe 1
  }

  it should "handle horizontal line, p2 below" in {
    val p0 = Point(0, 0)
    val p1 = Point(1, 0)
    val p2 = Point(0.5, -1) // below
    Point.ccw(p0, p1, p2) shouldBe -1
  }


