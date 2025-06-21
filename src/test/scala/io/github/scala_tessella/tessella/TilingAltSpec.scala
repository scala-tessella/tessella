package io.github.scala_tessella.tessella

import Geometry.Point
import RegularPolygon.Polygon
import Topology.{--, Edge, Node}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingAltSpec extends AnyFlatSpec with Matchers:

  "An empty TilingAlt" should "have no elements" in {
    val emptyTiling = TilingAlt.empty
    emptyTiling shouldBe a [TilingAlt]
    emptyTiling.edges shouldBe empty
    emptyTiling.orientedPolygons shouldBe empty
    emptyTiling.perimeter shouldBe empty
    emptyTiling.coordinates shouldBe empty
  }

  val square: TilingAlt =
    TilingAlt.fromPolygon(4)

  "A TilingAlt from a polygon" should "be a single regular polygon" in {
    square shouldBe a [TilingAlt]
    square.edges should have size 4
    square.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1)

    square.orientedPolygons should have size 1
    square.orientedPolygons.head should contain theSameElementsInOrderAs Vector(1, 2, 3, 4)

    square.perimeter should contain theSameElementsInOrderAs Vector(1, 2, 3, 4)

    square.coordinates should have size 4
    square.coordinates(Node(1)).almostEquals(Point(0.0, 0.0)) shouldBe true
    square.coordinates(Node(2)).almostEquals(Point(0.0, 1.0)) shouldBe true
    square.coordinates(Node(3)).almostEquals(Point(1.0, 1.0)) shouldBe true
    square.coordinates(Node(4)).almostEquals(Point(1.0, 0.0)) shouldBe true
  }

  it should "be created either from a Polygon or an Int" in {
    TilingAlt.fromPolygon(Polygon(6)) shouldBe TilingAlt.fromPolygon(6)
  }

  it should "throw an exception for polygons with less than 3 sides" in {
    an [IllegalArgumentException] should be thrownBy TilingAlt.fromPolygon(2)
  }

  "The TilingAlt companion object" should "correctly calculate coords for a square added to a square" in {
    val (newPolygonPath, newCoords) = TilingAlt.calculateNewPolygonCoords(
      square,
      Polygon(4),
      3--4
    )

    newPolygonPath should contain theSameElementsInOrderAs Vector(6, 5, 4, 3)

    newCoords should have size 2
    newCoords(Node(5)).almostEquals(Point(2.0, 0.0)) shouldBe true
    newCoords(Node(6)).almostEquals(Point(2.0, 1.0)) shouldBe true
  }

  it should "correctly calculate coords for a triangle added to a square" in {
    val (newPolygonPath, newCoords) = TilingAlt.calculateNewPolygonCoords(
      square,
      Polygon(3),
      1--2
    )

    newPolygonPath should contain theSameElementsInOrderAs Vector(5, 2, 1)

    newCoords should have size 1
    newCoords(Node(5)).almostEquals(Point(-0.8660254037844386, 0.5)) shouldBe true
  }

  it should "handle building on a reversed perimeter edge" in {
    val (newPolygonPath, newCoords) = TilingAlt.calculateNewPolygonCoords(
      square,
      Polygon(4),
      4--1
    )

    newPolygonPath should contain theSameElementsInOrderAs Vector(6, 5, 1, 4)

    newCoords should have size 2
    newCoords(Node(5)).almostEquals(Point(0.0, -1.0)) shouldBe true
    newCoords(Node(6)).almostEquals(Point(1.0, -1.0)) shouldBe true
  }

  it should "throw an AssertionError for a non-perimeter edge" in {
    an [AssertionError] should be thrownBy TilingAlt.calculateNewPolygonCoords(
      square,
      Polygon(4),
      1--3 // Diagonal, not on perimeter
    )
  }

  "The addPolygon method" should "add a square to another square" in {
    val result = TilingAlt.addPolygon(square, Polygon(4), 3--4)
    result.isRight shouldBe true
    val twoSquares = result.getOrElse(fail("Expected a TilingAlt"))

    twoSquares.edges should have size 7
    twoSquares.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1, 4--5, 5--6, 6--3)

    twoSquares.orientedPolygons should have size 2
    twoSquares.orientedPolygons should contain(Vector(1, 2, 3, 4))
    twoSquares.orientedPolygons should contain(Vector(6, 5, 4, 3))

    twoSquares.perimeter should have size 6
    twoSquares.perimeter should contain theSameElementsInOrderAs Vector(1, 2, 3, 6, 5, 4)

    twoSquares.coordinates should have size 6
    twoSquares.coordinates(Node(5)).almostEquals(Point(2.0, 0.0)) shouldBe true
    twoSquares.coordinates(Node(6)).almostEquals(Point(2.0, 1.0)) shouldBe true
  }

  it should "add a triangle to a square" in {
    val result = TilingAlt.addPolygon(square, Polygon(3), 1--2)
    result.isRight shouldBe true
    val squareAndTriangle = result.getOrElse(fail("Expected a TilingAlt"))

    squareAndTriangle.edges should have size 6
    squareAndTriangle.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1, 2--5, 5--1)

    squareAndTriangle.orientedPolygons should have size 2
    squareAndTriangle.orientedPolygons should contain(Vector(1, 2, 3, 4))
    squareAndTriangle.orientedPolygons should contain(Vector(5, 2, 1))

    squareAndTriangle.perimeter should have size 5
    squareAndTriangle.perimeter should contain theSameElementsInOrderAs Vector(2, 3, 4, 1, 5)

    squareAndTriangle.coordinates should have size 5
    squareAndTriangle.coordinates(Node(5)).almostEquals(Point(-0.8660254037844386, 0.5)) shouldBe true
  }

  it should "return an error when adding to a non-perimeter edge" in {
    val result = TilingAlt.addPolygon(square, Polygon(4), 1--3)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Expected an error message")) shouldBe "Perimeter edge not found."
  }