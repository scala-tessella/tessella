package io.github.scala_tessella.tessella

import Geometry.Point
import RegularPolygon.Polygon
import Topology.{--, Edge, Node}
import conversion.SVG.*
import SpireGeometry.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IncrementalTilingSpec extends AnyFlatSpec with Matchers:

  "An empty TilingAlt" should "have no elements" in {
    val emptyTiling = IncrementalTiling.empty
    emptyTiling shouldBe a [IncrementalTiling]
    emptyTiling.edges shouldBe empty
    emptyTiling.orientedPolygons shouldBe empty
    emptyTiling.perimeter shouldBe empty
    emptyTiling.coordinates shouldBe empty
    emptyTiling.isEmpty shouldBe true
  }

  val square: IncrementalTiling =
    IncrementalTiling.fromPolygon(4)

  "A TilingAlt from a polygon" should "be a single regular polygon" in {
    square shouldBe a [IncrementalTiling]
    square.edges should have size 4
    square.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1)

    square.orientedPolygons should have size 1
    square.orientedPolygons.head should contain theSameElementsInOrderAs Vector(1, 2, 3, 4)

    square.perimeter should contain theSameElementsInOrderAs Vector(1, 2, 3, 4)

    square.coordinates should have size 4
    square.coordinates(Node(1)).almostEquals(SpirePoint(0.0, 0.0)) shouldBe true
    square.coordinates(Node(2)).almostEquals(SpirePoint(0.0, 1.0)) shouldBe true
    square.coordinates(Node(3)).almostEquals(SpirePoint(1.0, 1.0)) shouldBe true
    square.coordinates(Node(4)).almostEquals(SpirePoint(1.0, 0.0)) shouldBe true
  }

  it should "be created either from a Polygon or an Int" in {
    IncrementalTiling.fromPolygon(Polygon(6)) shouldBe IncrementalTiling.fromPolygon(6)
  }

  it should "throw an exception for polygons with less than 3 sides" in {
    an [IllegalArgumentException] should be thrownBy IncrementalTiling.fromPolygon(2)
  }

  "The TilingAlt companion object" should "correctly calculate coords for a square added to a square" in {
    val (newPolygonPath, newCoords) =
      square.calculateNewPolygonCoords(Polygon(4), 3--4)

    newPolygonPath should contain theSameElementsInOrderAs Vector(-2, -1, 4, 3)

    newCoords should have size 2
    newCoords(Node(-1)).almostEquals(SpirePoint(2.0, 0.0)) shouldBe true
    newCoords(Node(-2)).almostEquals(SpirePoint(2.0, 1.0)) shouldBe true
  }

  it should "correctly calculate coords for a triangle added to a square" in {
    val (newPolygonPath, newCoords) =
      square.calculateNewPolygonCoords(Polygon(3), 1--2)

    newPolygonPath should contain theSameElementsInOrderAs Vector(-1, 2, 1)

    newCoords should have size 1
    newCoords(Node(-1)).almostEquals(SpirePoint(-0.8660254037844386, 0.5)) shouldBe true
  }

  it should "handle building on a reversed perimeter edge" in {
    val (newPolygonPath, newCoords) =
      square.calculateNewPolygonCoords(Polygon(4), 4--1)

    newPolygonPath should contain theSameElementsInOrderAs Vector(-2, -1, 1, 4)

    newCoords should have size 2
    newCoords(Node(-1)).almostEquals(SpirePoint(0.0, -1.0)) shouldBe true
    newCoords(Node(-2)).almostEquals(SpirePoint(1.0, -1.0)) shouldBe true
  }

  it should "throw an AssertionError for a non-perimeter edge" in {
    an [AssertionError] should be thrownBy
      square.calculateNewPolygonCoords(Polygon(4), 1--3) // Diagonal, not on perimeter
  }
