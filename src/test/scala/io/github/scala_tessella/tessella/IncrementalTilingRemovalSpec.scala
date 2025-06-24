package io.github.scala_tessella.tessella


import RegularPolygon.Polygon
import Topology.{--, Node}
import conversion.SVG.*

import io.github.scala_tessella.ring_seq.RingSeq.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IncrementalTilingRemovalSpec extends AnyFlatSpec with Matchers:

  val square: IncrementalTiling =
    IncrementalTiling.fromPolygon(4)

  // Helper tiling: two squares adjacent to each other.
  val twoSquares: IncrementalTiling =
    square.addPolygon(Polygon(4), 3--4).getOrElse(fail("Setup failed: could not create twoSquares"))

  "The removePolygon method" should "remove a polygon from a tiling, resulting in the original tiling" in {
    val removedPolygonPath = Vector(6, 5, 4, 3).map(Node(_)) // The second square
    val result = twoSquares.removePolygon(removedPolygonPath)

    result.isRight shouldBe true
    val oneSquare = result.getOrElse(fail("Expected a TilingAlt after removal"))

    // The result should be identical to the original single square
    oneSquare.edges should contain theSameElementsAs square.edges
    oneSquare.orientedPolygons.head should contain theSameElementsInOrderAs square.orientedPolygons.head
    oneSquare.perimeter should contain theSameElementsInOrderAs square.perimeter
    oneSquare.coordinates.keys should contain theSameElementsAs square.coordinates.keys
  }

  it should "remove a polygon regardless of its path orientation" in {
    val removedPolygonPathReversed = Vector(3, 4, 5, 6).map(Node(_)) // Same polygon, different orientation
    val result = twoSquares.removePolygon(removedPolygonPathReversed)

    result.isRight shouldBe true
    val oneSquare = result.getOrElse(fail("Expected a TilingAlt after removal"))
    result.getOrElse(fail()).edges should have size 4
    // The result should be identical to the original single square
    oneSquare.edges should contain theSameElementsAs square.edges
    oneSquare.orientedPolygons.head should contain theSameElementsInOrderAs square.orientedPolygons.head
    oneSquare.perimeter should contain theSameElementsInOrderAs square.perimeter
    oneSquare.coordinates.keys should contain theSameElementsAs square.coordinates.keys
  }

  it should "return an error if the polygon to remove is not found" in {
    val nonExistentPolygon = Vector(1, 5, 6, 7).map(Node(_))
    val result = twoSquares.removePolygon(nonExistentPolygon)

    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe
      "Invalid shared separate nodes: 1. Polygon with edges 1--5--6--7-- shares edges 5--6 with perimeter 1--2--3--6--5--4--."
  }

  // Tiling where one square is completely surrounded by others
  val enclosedSquareTiling: IncrementalTiling =
    IncrementalTiling.fromPolygon(4) // Central square (1,2,3,4)
      .addPolygon(Polygon(4), 1--2).getOrElse(fail()) // Top square
      .addPolygon(Polygon(4), 2--3).getOrElse(fail()) // Right square
      .addPolygon(Polygon(4), 3--4).getOrElse(fail()) // Bottom square
      .addPolygon(Polygon(4), 4--1).getOrElse(fail()) // Left square

  it should "return an error when trying to remove an internal polygon" in {
    val internalPolygon = Vector(1, 2, 3, 4).map(Node(_))
    val result = enclosedSquareTiling.removePolygon(internalPolygon)

    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe
      "Polygon with edges 1--2--3--4-- doesn't share any with perimeter 6--5--2--8--7--3--10--9--4--12--11--1--."
  }

  // Tiling for testing disconnected edges: a square with two triangles on opposite sides
  val squareWithTwoTriangles: IncrementalTiling =
    IncrementalTiling.fromPolygon(4)
      .addPolygon(Polygon(3), 1--2).getOrElse(fail())
      .addPolygon(Polygon(3), 3--4).getOrElse(fail())

  it should "return an error if the polygon shares non-continuous edges with the perimeter" in {
    // The square (1,2,3,4) touches the perimeter at edges 2--3 and 4--1.
    // These two segments of the perimeter are separated by the triangles.
    val result = squareWithTwoTriangles.removePolygon(Vector(1, 2, 3, 4).map(Node(_)))
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Non-continuos shared edges. Polygon with edges 1--2--3--4-- shares edges 2--3, 1--4 with perimeter 1--5--2--3--6--4--."
  }

  val threeTriangles: IncrementalTiling =
    IncrementalTiling.fromPolygon(3)
      .addPolygon(Polygon(3), 1--2).getOrElse(fail())
      .addPolygon(Polygon(3), 1--3).getOrElse(fail())

  it should "return an error if the polygon shares one node outside the shared edges with the perimeter" in {
    // The square (1,2,3,4) touches the perimeter at edges 2--3 and 4--1.
    // These two segments of the perimeter are separated by the triangles.
    val result = threeTriangles.removePolygon(Vector(1, 2, 3).map(Node(_)))
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Invalid shared separate nodes: 1. Polygon with edges 1--2--3-- shares edges 2--3 with perimeter 4--2--3--5--1--."
  }

  it should "remove the last polygon, resulting in an empty tiling" in {
    val singleSquare = IncrementalTiling.fromPolygon(4)
    val result = singleSquare.removePolygon(Vector(1, 2, 3, 4).map(Node(_)))
    result.isRight shouldBe true
    val emptyTiling = result.getOrElse(fail())
    emptyTiling shouldBe IncrementalTiling.empty
  }

  // Setup for more complex tilings from the add specs
  val dodecagon: IncrementalTiling =
    IncrementalTiling.fromPolygon(12)

  val dodecagonAndTriangle: IncrementalTiling =
    dodecagon.addPolygon(Polygon(3), 1--2).getOrElse(fail())

  it should "remove a polygon sharing two edges with the perimeter" in {
    // The triangle (1,2,13) is added to the dodecagon (1-12) on edge 1--2
    // Its other two edges, 1--13 and 2--13, form part of the new perimeter
    val trianglePath = Vector(1, 2, 13).map(Node(_))
    val result = dodecagonAndTriangle.removePolygon(trianglePath)
    result.isRight shouldBe true
    val originalDodecagon = result.getOrElse(fail())

    // The result should be the original dodecagon
    originalDodecagon.edges.size shouldBe dodecagon.edges.size
    originalDodecagon.perimeter.size shouldBe dodecagon.perimeter.size
    originalDodecagon.edges should contain theSameElementsAs dodecagon.edges
    originalDodecagon.perimeter.isRotationOf(dodecagon.perimeter) shouldBe true
  }

  val twoDodecagonsAndTriangle: IncrementalTiling =
    dodecagonAndTriangle.addPolygon(Polygon(12), 1--13).getOrElse(fail())

  it should "remove a polygon from a complex tiling with multiple shared edges" in {
    // In this tiling, the triangle (1,2,13) now only shares ONE edge (2--13) with the perimeter
    val trianglePath = Vector(1, 2, 13).map(Node(_))
    val result = twoDodecagonsAndTriangle.removePolygon(trianglePath)
    result.isRight shouldBe true
    val newTiling = result.getOrElse(fail())

    // Expected perimeter after removing the triangle:
    val expectedPerimeter =
      Vector(3, 4, 5, 6, 7, 8, 9, 10, 11, 12) ++
        Vector(22, 21, 20, 19, 18, 17, 16, 15, 14, 13)
        ++ Vector(1, 2)

    newTiling.orientedPolygons.size shouldBe 2
    newTiling.perimeter should contain theSameElementsInOrderAs expectedPerimeter
  }

  val cShape: IncrementalTiling =
    IncrementalTiling.fromPolygon(4) // 1,2,3,4
      .addPolygon(Polygon(4), 4--1).getOrElse(fail()) // 5,6,1,4
      .addPolygon(Polygon(4), 1--2).getOrElse(fail()) // 7,8,2,1

  it should "remove a polygon from a more complex shape (C-shape)" in {
    val polygonToRemove = Vector(6, 5, 1, 4).map(Node(_))
    val result = cShape.removePolygon(polygonToRemove)
    println(result)
    result.isRight shouldBe true
    val resultingTiling = result.getOrElse(fail())

    resultingTiling.edges should contain theSameElementsAs List(1--2, 2--3, 3--4, 4--1, 7--8, 2--7, 1--8)
    resultingTiling.orientedPolygons.map(_.toSet) should contain theSameElementsAs List(Set(1, 2, 3, 4), Set(8, 7, 2, 1))
    resultingTiling.perimeter should contain theSameElementsInOrderAs Vector(8, 7, 2, 3, 4, 1)
  }
