package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU_4
import Topology.{--, Edge, Node}
import GeometryBase.{Box9D, LineSegment9D, Point9D}
import math.geom2d.{Box2D, Point2D}
import math.geom2d.Shape2D.ACCURACY
import math.geom2d.line.LineSegment2D
import math.geom2d.polygon.SimplePolygon2D
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.jdk.CollectionConverters.*

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

  val point: Point2D =
    Point2D(1.0, -1.0)

  val almost: Point2D =
    Point2D(0.9999999999999998, -1.0)

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
    Point9D.fromPoint2D(point).rounded shouldBe
      (1000000L, -1000000L)
  }

  it can "return a new point moved by polar coords" in {
    Point9D.fromPoint2D(point).plusPolar(2.0)(TAU_4).almostEquals(Point9D(1.0, 1.0), ACCURACY) shouldBe
      true
  }

  it can "return a new point moved by 1 unit in a polar direction" in {
    Point9D.fromPoint2D(point).plusPolarUnit(TAU_4) shouldBe
      Point9D(1.0, 0.0)
  }

  it can "return the angle to another point" in {
    Point9D.fromPoint2D(point).angleTo(Point9D(1.0, 0.0)) shouldBe
      TAU_4
  }

  it can "be aligned to other two points" in {
    Point9D.fromPoint2D(point).alignWithStart(Point9D(), Point9D(1.0, 0.0)).almostEquals(Point9D.fromPoint2D(point), ACCURACY) shouldBe
      true
  }

  it can "be flipped vertically" in {
    Point9D.fromPoint2D(point).flipVertically shouldBe
      Point9D(1.0, 1.0)
  }

  val points: Vector[Point2D] =
    Vector(point, Point2D(), Point2D(3.0, 3.0), almost)

  "A set of Point2D" can "have no duplicates, with approximation" in {
    points.map(Point9D.fromPoint2D).tail.areAllDistinct shouldBe
      true
  }

  it can "have duplicates, with approximation, when an almost equal pont is added" in {
    points.map(Point9D.fromPoint2D).areAllDistinct shouldBe
      false
  }

  it can "return only the duplicates, with approximation" in {
    points.map(Point9D.fromPoint2D).almostEqualCouples.toList shouldBe
      List((point, almost)).map((p1, p2) => (Point9D.fromPoint2D(p1), Point9D.fromPoint2D(p2)))
  }

  it can "be approximately equal to another set" in {
    points.map(Point9D.fromPoint2D).almostEquals(Vector(almost, Point2D(), Point2D(3.0, 3.0), point).map(Point9D.fromPoint2D)) shouldBe
      true
  }

  val segment: LineSegment2D =
    LineSegment2D(Point2D(), point)

  val intersecting: LineSegment2D =
    LineSegment2D(Point2D(0.0, -1.0), Point2D(1.0, 0.0))

  val sameOrigin: LineSegment2D =
    LineSegment2D(Point2D(), Point2D(3.0, 3.0))

  "A LineSegment9D" can "intersect with another one" in {
    LineSegment9D.intersects(LineSegment9D.fromLineSegment2D(segment), LineSegment9D.fromLineSegment2D(intersecting)) shouldBe
      true
  }

  it can "return the point of intersection" in {
    LineSegment9D.fromLineSegment2D(segment).intersection(LineSegment9D.fromLineSegment2D(intersecting)).get shouldBe
      Point9D(0.5, -0.5)
  }

  it can "lesser intersect with another one" in {
    LineSegment9D.fromLineSegment2D(segment).lesserIntersects(LineSegment9D.fromLineSegment2D(intersecting)) shouldBe
      true
  }

  it can "contain a point" in {
    LineSegment9D.fromLineSegment2D(segment).containsAtEdges(Point9D.fromPoint2D(sameOrigin.firstPoint())) shouldBe
      true
  }

  it can "NOT contain a point" in {
    LineSegment9D.fromLineSegment2D(segment).containsAtEdges(Point9D.fromPoint2D(sameOrigin.lastPoint())) shouldBe
      false
  }

  "A LineSegment2D" can "intersect with another one" in {
    LineSegment2D.intersects(segment, intersecting) shouldBe
      true
  }

  it can "lesser intersect with another one" in {
    LineSegment9D.fromLineSegment2D(segment).lesserIntersects(LineSegment9D.fromLineSegment2D(intersecting)) shouldBe
      true
  }

  it must "not lesser intersect if just one endpoint is shared" in {
    LineSegment9D.fromLineSegment2D(segment).lesserIntersects(LineSegment9D.fromLineSegment2D(sameOrigin)) shouldBe
      false
  }

  it can "return the point of intersection" in {
    segment.intersection(intersecting) shouldBe
      Point2D(0.5, -0.5)
  }

  it can "have at least one of its two endpoints contained in a Box2D" in {
    LineSegment9D.fromLineSegment2D(segment).hasEndpointIn(Box9D.fromBox2D(Box2D(Point2D(-2.0, 0.0), Point2D(1.0, 1.0)))) shouldBe
      true
  }

  it can "have both of its two endpoints NOT contained in a Box2D" in {
    LineSegment9D.fromLineSegment2D(segment).hasEndpointIn(Box9D.fromBox2D(Box2D(Point2D(0.5, 0.5), Point2D(1.0, 1.0)))) shouldBe
      false
  }

  it can "contain a point" in {
    segment.contains(sameOrigin.firstPoint()) shouldBe
      true
  }

  it can "NOT contain a point" in {
    segment.contains(sameOrigin.lastPoint()) shouldBe
      false
  }

  val simple: SimplePolygon2D =
    SimplePolygon2D(List(
      Point2D(),
      Point2D(1.0, 1.0),
      Point2D(1.0, 0.0),
      Point2D(0.0, 1.0)
    ).asJava)

  "A SimplePolygon2D" can "be self intersecting" in {
    simple.isSelfIntersecting shouldBe
      true
  }

  it can "return the couples of segments self intersecting" in {
    simple.intersectingSides.toList shouldBe
      List(
        (
          LineSegment9D(Point9D(1.0, 0.0), Point9D(0.0, 1.0)),
          LineSegment9D(Point9D(), Point9D(1.0, 1.0))
        )
      )
  }

  "A set of LineSegment2D" can "be approximately equal to another set" in {
    List(segment, sameOrigin).map(LineSegment9D.fromLineSegment2D)
      .almostEquals(List(LineSegment2D(Point2D(), almost), sameOrigin).map(LineSegment9D.fromLineSegment2D)) shouldBe
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

//  it can "be created incorrect with a deprecated method" in {
//    List(57--68, 55--68).toBoxOld(someCoords) shouldEqual
//      Box2D(-0.3660254037849453, 0.49999999999960043, -1.3660254037838226, 4.9E-324)
//  }

}
