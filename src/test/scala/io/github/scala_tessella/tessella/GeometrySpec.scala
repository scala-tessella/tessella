package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU_4
import GeometryBase.{ACCURACY, Box9D, LineSegment9D, Point9D, RegularPolygon2D, SimplePolygon9D}
import Topology.{--, Edge, Node}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GeometrySpec extends AnyFlatSpec with Helper with should.Matchers {

  "A radian" can "be created as an opaque type" in {
    Radian(0).toString shouldBe "0.0"
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

  val point: Point9D =
    Point9D(1.0, -1.0)

  val almost: Point9D =
    Point9D(0.9999999999999998, -1.0)

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

  it can "be rounded to couple of Long" in {
    point.rounded shouldBe
      (1000000L, -1000000L)
  }

  it can "return a new point moved by polar coords" in {
    point.plusPolar(2.0)(TAU_4).almostEquals(Point9D(1.0, 1.0), ACCURACY) shouldBe
      true
  }

  it can "return a new point moved by 1 unit in a polar direction" in {
    point.plusPolarUnit(TAU_4) shouldBe
      Point9D(1.0, 0.0)
  }

  it can "return the angle to another point" in {
    point.angleTo(Point9D(1.0, 0.0)) shouldBe
      TAU_4
  }

  it can "be aligned to other two points" in {
    point.alignWithStart(Point9D(), Point9D(1.0, 0.0)).almostEquals(point, ACCURACY) shouldBe
      true
  }

  it can "be flipped vertically" in {
    point.flipVertically shouldBe
      Point9D(1.0, 1.0)
  }

  val points: Vector[Point9D] =
    Vector(point, Point9D(), Point9D(3.0, 3.0), almost)

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
    points.almostEquals(Vector(almost, Point9D(), Point9D(3.0, 3.0), point)) shouldBe
      true
  }

  val segment: LineSegment9D =
    LineSegment9D(Point9D(), point)

  val intersecting: LineSegment9D =
    LineSegment9D(Point9D(0.0, -1.0), Point9D(1.0, 0.0))

  val sameOrigin: LineSegment9D =
    LineSegment9D(Point9D(), Point9D(3.0, 3.0))

  "A LineSegment9D" can "intersect with another one" in {
    LineSegment9D.intersects(segment, intersecting) shouldBe
      true
  }

  it can "return the point of intersection" in {
    segment.intersection(intersecting).get shouldBe
      Point9D(0.5, -0.5)
  }

  it can "lesser intersect with another one" in {
    segment.lesserIntersects(intersecting) shouldBe
      true
  }

  it can "contain a point" in {
    segment.containsAtEdges(sameOrigin.point1) shouldBe
      true
  }

  it can "NOT contain a point" in {
    segment.containsAtEdges(sameOrigin.point2) shouldBe
      false
  }

  "A LineSegment2D" can "intersect with another one" in {
    LineSegment9D.intersects(segment, intersecting) shouldBe
      true
  }

  it can "have at least one of its two endpoints contained in a Box2D" in {
    segment.hasEndpointIn(Box9D(-2.0, 1.0, 0.0, 1.0)) shouldBe
      true
  }

  it can "have both of its two endpoints NOT contained in a Box2D" in {
    segment.hasEndpointIn(Box9D(0.5, 1.0, 0.5, 1.0)) shouldBe
      false
  }

  val single: SimplePolygon9D =
    SimplePolygon9D(List(
      Point9D(),
      Point9D(1.0, 1.0),
      Point9D(1.0, 0.0),
      Point9D(0.0, 1.0)
    ))

  "A SimplePolygon2D" can "be self intersecting" in {
    single.isSelfIntersecting shouldBe
      true
  }

  it can "return the couples of segments self intersecting" in {
    single.intersectingSides.toList shouldBe
      List(
        (
          LineSegment9D(Point9D(1.0, 0.0), Point9D(0.0, 1.0)),
          LineSegment9D(Point9D(), Point9D(1.0, 1.0))
        )
      )
  }

  val regular: RegularPolygon2D =
    RegularPolygon2D(List(
      Point9D(),
      Point9D(0.0, 1.0),
      Point9D(1.0, 1.0),
      Point9D(1.0, 0.0)
    ))

  "A regular polygon" must "have a centre" in {
    regular.center() shouldBe
      Point9D(0.5, 0.5)
  }

  "A set of LineSegment2D" can "be approximately equal to another set" in {
    List(segment, sameOrigin)
      .almostEquals(List(LineSegment9D(Point9D(), almost), sameOrigin)) shouldBe
      true
  }

  val coords: Coords =
    Map(
      Node(1) -> Point9D(0.0, -2.0),
      Node(2) -> Point9D(1.0, -2.0),
      Node(3) -> Point9D(1.0, -1.0),
      Node(4) -> Point9D(0.0, -1.0)
    )

  "A set of coordinates" can "be flipped vertically if node 3 is negative on the y-axis" in {
    coords.flipVertically shouldBe
      Map(
        1 -> Point9D(0.0, 2.0),
        2 -> Point9D(1.0, 2.0),
        3 -> Point9D(1.0, 1.0),
        4 -> Point9D(0.0, 1.0)
      )
  }

  it can "be aligned to the two start points" in {
    coords.alignWithStart.almostEqualsMap(Map(
      Node(1) -> Point9D(0.0, 0.0),
      Node(2) -> Point9D(1.0, 0.0),
      Node(3) -> Point9D(1.0, 1.0),
      Node(4) -> Point9D(0.0, 1.0)
    )) shouldBe
      true
  }

  val anEdge: Edge =
    1--2

  "An Edge" can "be converted to a LineSegment9D" in {
    anEdge.toSegment(Map(Node(1) -> Point9D(), Node(2) -> Point9D(1.0, 0.0))) shouldBe
      LineSegment9D(Point9D(), Point9D(1.0, 0.0))
  }

  val second: Edge =
    3--1

  it can "be converted to LineSegment2D" in {
    List(anEdge, second).toSegments(Map(
      Node(1) -> Point9D(),
      Node(2) -> Point9D(1.0, 0.0),
      Node(3) -> Point9D(1.0, 1.0),
    )) shouldEqual
      List(
        LineSegment9D(Point9D(), Point9D(1.0, 0.0)),
        LineSegment9D(Point9D(), Point9D(1.0, 1.0))
      )
  }

  it can "be converted to Box" in {
    List(anEdge, second).toBox(Map(
      Node(1) -> Point9D(-9.0, 10.0),
      Node(2) -> Point9D(11.0, 10.0),
      Node(3) -> Point9D(11.0, 11.0),
    )) shouldEqual
      Box9D(-9.0, 11.0, 10.0, 11.0)
  }

  val someCoords: Coords =
    Map(
      Node(55) -> Point9D(0.49999999999960043, -0.866025403784008),
      Node(57) -> Point9D(-0.3660254037849453, -1.3660254037838226),
      Node(68) -> Point9D(-0.3660254037847309, -0.36602540378382264)
    )

  "Another box" can "be created" in {
    List(57--68, 55--68).toBox(someCoords) shouldEqual
      Box9D(-0.3660254037849453, 0.49999999999960043, -1.3660254037838226, -0.36602540378382264)
  }

  "A regular triangle" must "have a center" in {
    RegularPolygon2D(someCoords.values.toList).center() shouldBe
      Point9D(-0.07735026919002526, -0.8660254037838845)
  }

//  it can "be created incorrect with a deprecated method" in {
//    List(57--68, 55--68).toBoxOld(someCoords) shouldEqual
//      Box2D(-0.3660254037849453, 0.49999999999960043, -1.3660254037838226, 4.9E-324)
//  }

}
