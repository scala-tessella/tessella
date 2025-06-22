package io.github.scala_tessella.tessella

import Geometry.Point
import RegularPolygon.Polygon
import Topology.{--, Edge, Node}
import conversion.SVG.*

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
    square.coordinates(Node(1)).almostEquals(Point(0.0, 0.0)) shouldBe true
    square.coordinates(Node(2)).almostEquals(Point(0.0, 1.0)) shouldBe true
    square.coordinates(Node(3)).almostEquals(Point(1.0, 1.0)) shouldBe true
    square.coordinates(Node(4)).almostEquals(Point(1.0, 0.0)) shouldBe true
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

    newPolygonPath should contain theSameElementsInOrderAs Vector(6, 5, 4, 3)

    newCoords should have size 2
    newCoords(Node(5)).almostEquals(Point(2.0, 0.0)) shouldBe true
    newCoords(Node(6)).almostEquals(Point(2.0, 1.0)) shouldBe true
  }

  it should "correctly calculate coords for a triangle added to a square" in {
    val (newPolygonPath, newCoords) =
      square.calculateNewPolygonCoords(Polygon(3), 1--2)

    newPolygonPath should contain theSameElementsInOrderAs Vector(5, 2, 1)

    newCoords should have size 1
    newCoords(Node(5)).almostEquals(Point(-0.8660254037844386, 0.5)) shouldBe true
  }

  it should "handle building on a reversed perimeter edge" in {
    val (newPolygonPath, newCoords) =
      square.calculateNewPolygonCoords(Polygon(4), 4--1)

    newPolygonPath should contain theSameElementsInOrderAs Vector(6, 5, 1, 4)

    newCoords should have size 2
    newCoords(Node(5)).almostEquals(Point(0.0, -1.0)) shouldBe true
    newCoords(Node(6)).almostEquals(Point(1.0, -1.0)) shouldBe true
  }

  it should "throw an AssertionError for a non-perimeter edge" in {
    an [AssertionError] should be thrownBy
      square.calculateNewPolygonCoords(Polygon(4), 1--3) // Diagonal, not on perimeter
  }

  "The addPolygon method" should "add a square to another square" in {
    val result = square.addPolygon(Polygon(4), 3--4)
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
    val result = square.addPolygon(Polygon(3), 1--2)
    result.isRight shouldBe true
    val squareAndTriangle = result.getOrElse(fail("Expected a TilingAlt"))

    squareAndTriangle.edges should have size 6
    squareAndTriangle.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1, 2--5, 5--1)

    squareAndTriangle.orientedPolygons should have size 2
    squareAndTriangle.orientedPolygons should contain(Vector(1, 2, 3, 4))
    squareAndTriangle.orientedPolygons should contain(Vector(5, 2, 1))

    squareAndTriangle.perimeter should have size 5
    squareAndTriangle.perimeter should contain theSameElementsInOrderAs Vector(3, 4, 1, 5, 2)

    squareAndTriangle.coordinates should have size 5
    squareAndTriangle.coordinates(Node(5)).almostEquals(Point(-0.8660254037844386, 0.5)) shouldBe true
  }

  it should "return an error when adding to a non-perimeter edge" in {
    val result = square.addPolygon(Polygon(4), 1--3)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Expected an error message")) shouldBe "Perimeter edge not found."
  }

  val dodecagon: IncrementalTiling =
    IncrementalTiling.fromPolygon(12)

  val dodecagonAndTriangle: IncrementalTiling =
    dodecagon.addPolygon(Polygon(3), 1--2).toOption.get

  it should "add a dodecagon sharing 2 perimeter edges to a triangle and another dodecagon" in {
    val result = dodecagonAndTriangle.addPolygon(Polygon(12), 1--13)
    result.isRight shouldBe true
    val t31212 = result.getOrElse(fail("Expected a TilingAlt"))

    t31212.edges should have size 24
    t31212.perimeter should contain theSameElementsInOrderAs
      Vector(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13)
  }

  it should "add a dodecagon sharing 2 perimeter edges to another dodecagon and a triangle" in {
    val result = dodecagonAndTriangle.addPolygon(Polygon(12), 1--12)
    result.isRight shouldBe true
    val t31212 = result.getOrElse(fail("Expected a TilingAlt"))

    t31212.edges should have size 24
    t31212.perimeter should contain theSameElementsInOrderAs
      Vector(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 23, 22, 21, 20, 19, 18, 17, 16, 15, 13)
  }

  val twoDodecagonsAndTriangle: IncrementalTiling =
    dodecagon
      .addPolygon(Polygon(3), 1--2).toOption.get
      .addPolygon(Polygon(12), 1--13).toOption.get

  it should "add a dodecagon sharing 3 perimeter edges to a triangle and other two dodecagons" in {
    val result = twoDodecagonsAndTriangle.addPolygon(Polygon(12), 2--13)
    result.isRight shouldBe true
    val t3121212 = result.getOrElse(fail("Expected a TilingAlt"))

    t3121212.edges should have size 33
    t3121212.perimeter should contain theSameElementsInOrderAs
      Vector(4, 5, 6, 7, 8, 9, 10, 11, 12, 22, 21, 20, 19, 18, 17, 16, 15, 14, 31, 30, 29, 28, 27, 26, 25, 24, 3)
  }

  it should "add a dodecagon sharing 3 perimeter edges to a dodecagon and a triangle and another dodecagon" in {
    val result = twoDodecagonsAndTriangle.addPolygon(Polygon(12), 2--3)
    result.isRight shouldBe true
    val t3121212 = result.getOrElse(fail("Expected a TilingAlt"))

    t3121212.edges should have size 33
    t3121212.perimeter should contain theSameElementsInOrderAs
      Vector(4, 5, 6, 7, 8, 9, 10, 11, 12, 22, 21, 20, 19, 18, 17, 16, 15, 14, 30, 29, 28, 27, 26, 25, 24, 23, 3)
  }

  // Tiling where one square is almost completely surrounded by others
  val quasiEnclosedSquareTiling: IncrementalTiling =
    square // Central square (1,2,3,4)
      .addPolygon(Polygon(4), 1--2).getOrElse(fail())
      .addPolygon(Polygon(4), 2--3).getOrElse(fail())
      .addPolygon(Polygon(4), 3--4).getOrElse(fail())

  it should "add 3 squares to a central square" in {
//    println(quasiEnclosedSquareTiling.toSVG().toString)
    quasiEnclosedSquareTiling.perimeter should have size 10
    quasiEnclosedSquareTiling.edges should have size 13
  }

  it should "add 4 squares to a central square" in {
    val result = quasiEnclosedSquareTiling.addPolygon(Polygon(4), 4--1)
    result.isRight shouldBe true
    val enclosedSquareTiling = result.getOrElse(fail("Expected a TilingAlt"))

    enclosedSquareTiling.perimeter should have size 12
  }

  val almostLoop: IncrementalTiling =
    square
      .addPolygon(Polygon(4), 1--2).getOrElse(fail())
      .addPolygon(Polygon(4), 3--4).getOrElse(fail())
      .addPolygon(Polygon(4), 2--5).getOrElse(fail())
      .addPolygon(Polygon(4), 3--8).getOrElse(fail())
      .addPolygon(Polygon(4), 9--10).getOrElse(fail())
      .addPolygon(Polygon(4), 11--12).getOrElse(fail())

  it should "add 1 square to close a loop, with two edges at the same coordinates" in {
    val result = almostLoop.addPolygon(Polygon(4), 9--13)
    result.isRight shouldBe true
    val loop = result.getOrElse(fail("Expected a TilingAlt"))
    loop.coordinates(Node(16)).almostEquals(loop.coordinates(Node(18))) shouldBe true
    loop.coordinates(Node(12)).almostEquals(loop.coordinates(Node(17))) shouldBe true
    loop.perimeter should have size 18
  }

