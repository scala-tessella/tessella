package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.Geometry.Point
import io.github.scala_tessella.tessella.IncrementalTiling.Strictness
import io.github.scala_tessella.tessella.IncrementalTiling.Strictness
import io.github.scala_tessella.tessella.RegularPolygon.Polygon
import io.github.scala_tessella.tessella.Topology.{--, Edge, Node, NodeOrdering}
import io.github.scala_tessella.tessella.conversion.SVG.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IncrementalTilingAdditionSpec extends AnyFlatSpec with Matchers:

  val square: IncrementalTiling =
    IncrementalTiling.fromPolygon(4)

  "The addPolygon method" should "add a square to another square" in {
    val result = square.addPolygon(Polygon(4), 3--4)
    result.isRight shouldBe true
    val twoSquares = result.getOrElse(fail("Expected a TilingAlt"))

    twoSquares.edges should have size 7
    twoSquares.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1, 5--6, 4--6, 3--5)

    twoSquares.orientedPolygons should have size 2
    twoSquares.orientedPolygons should contain(Vector(1, 2, 3, 4))
    twoSquares.orientedPolygons should contain(Vector(5, 6, 4, 3))

    twoSquares.perimeter should have size 6
    twoSquares.perimeter should contain theSameElementsInOrderAs Vector(1, 2, 3, 5, 6, 4)

    twoSquares.coordinates should have size 6
    twoSquares.coordinates(Node(6)).almostEquals(Point(2.0, 0.0)) shouldBe true
    twoSquares.coordinates(Node(5)).almostEquals(Point(2.0, 1.0)) shouldBe true
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
    result.left.getOrElse(fail("Expected an error message")) shouldBe "Perimeter edge 1--3 not found."
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
      Vector(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 13)
  }

  it should "add a dodecagon sharing 2 perimeter edges to another dodecagon and a triangle" in {
    val result = dodecagonAndTriangle.addPolygon(Polygon(12), 1--12)
    result.isRight shouldBe true
    val t31212 = result.getOrElse(fail("Expected a TilingAlt"))

    t31212.edges should have size 24
    t31212.perimeter should contain theSameElementsInOrderAs
      Vector(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 13)
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
      Vector(4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 3)
  }

  it should "add a dodecagon sharing 3 perimeter edges to a dodecagon and a triangle and another dodecagon" in {
    val result = twoDodecagonsAndTriangle.addPolygon(Polygon(12), 2--3)
    result.isRight shouldBe true
    val t3121212 = result.getOrElse(fail("Expected a TilingAlt"))

    t3121212.edges should have size 33
    t3121212.perimeter should contain theSameElementsInOrderAs
      Vector(4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 3)
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
      .addPolygon(Polygon(4), 2--6).getOrElse(fail())
      .addPolygon(Polygon(4), 3--7).getOrElse(fail())
      .addPolygon(Polygon(4), 9--10).getOrElse(fail())
      .addPolygon(Polygon(4), 11--12).getOrElse(fail())

  it should "add 1 square to close a loop, with two edges at the same coordinates, if Strictness.TOUCHING" in {
    val result = almostLoop.addPolygon(Polygon(4), 10--14, Strictness.TOUCHING)
    result.isRight shouldBe true
    val loop = result.getOrElse(fail("Expected a TilingAlt"))
    loop.coordinates(Node(11)).almostEquals(loop.coordinates(Node(18))) shouldBe true
    loop.coordinates(Node(15)).almostEquals(loop.coordinates(Node(17))) shouldBe true
    loop.perimeter should have size 18
  }

  it should "fail to add 1 square to close a loop, with two edges at the same coordinates" in {
    val result = almostLoop.addPolygon(Polygon(4), 10--14)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Expected an error message")) shouldBe "Coincident nodes 11, 15 outside of the shared edges."
  }

  "The addPolygon method with perimeter crossing" should "fail with STRICT" in {
    val result = almostLoop.addPolygon(Polygon(6), 10--14, Strictness.STRICT)
    result.isLeft shouldBe true
    result.left.getOrElse(fail("Expected an error message")) shouldBe "Invalid addition: new polygon's edges cross perimeter."
  }

  it should "succeed with CROSSING" in {
    val result = almostLoop.addPolygon(Polygon(6), 10--14, Strictness.CROSSING)
    result.isRight shouldBe true
  }

  val twoHexagons: IncrementalTiling =
    IncrementalTiling.fromPolygon(6)
      .addPolygon(Polygon(6), 1--2).getOrElse(fail())

  "The addPolygon method" should "follow the node numbering without gaps" in {
    val result = twoHexagons.addPolygon(Polygon(6), 1--7)
    result.isRight shouldBe true
    val nodes = result.getOrElse(fail()).edges.nodes
    nodes should have size nodes.max(NodeOrdering).toInt
  }

  it should "follow it even when the polygon is added to the other edge" in {
    val result = twoHexagons.addPolygon(Polygon(6), 1--6)
    result.isRight shouldBe true
    val nodes = result.getOrElse(fail()).edges.nodes
    nodes should have size nodes.max(NodeOrdering).toInt
  }
