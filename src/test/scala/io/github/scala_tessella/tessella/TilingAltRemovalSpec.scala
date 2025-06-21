package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.RegularPolygon.Polygon
import io.github.scala_tessella.tessella.Topology.{--, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TilingAltRemovalSpec extends AnyFlatSpec with Matchers:

  val square: TilingAlt =
    TilingAlt.fromPolygon(4)

  // Helper tiling: two squares adjacent to each other.
  val twoSquares: TilingAlt =
    square.addPolygon(Polygon(4), 3--4).getOrElse(fail("Setup failed: could not create twoSquares"))

//  "The removePolygon method" should "remove a polygon from a tiling, resulting in the original tiling" in {
//      val removedPolygonPath = Vector(6, 5, 4, 3).map(Node(_)) // The second square
//      val result = twoSquares.removePolygon(removedPolygonPath)
//
//      result.isRight shouldBe true
//      val oneSquare = result.getOrElse(fail("Expected a TilingAlt after removal"))
//
//      // The result should be identical to the original single square
//      oneSquare.edges should contain theSameElementsAs square.edges
//      oneSquare.orientedPolygons.head should contain theSameElementsInOrderAs square.orientedPolygons.head
//      oneSquare.perimeter should contain theSameElementsInOrderAs square.perimeter
//      oneSquare.coordinates.keys should contain theSameElementsAs square.coordinates.keys
//  }
//
//  it should "remove a polygon regardless of its path orientation" in {
//      val removedPolygonPathReversed = Vector(3, 4, 5, 6).map(Node(_)) // Same polygon, different orientation
//      val result = twoSquares.removePolygon(removedPolygonPathReversed)
//
//      result.isRight shouldBe true
//      result.getOrElse(fail()).edges should have size 4
//    }

  it should "return an error if the polygon to remove is not found" in {
      val nonExistentPolygon = Vector(1, 5, 6, 7).map(Node(_))
      val result = twoSquares.removePolygon(nonExistentPolygon)

      result.isLeft shouldBe true
      result.left.getOrElse(fail()) shouldBe "Polygon not found."
    }

    // Tiling where one square is completely surrounded by others
  val enclosedSquareTiling: TilingAlt =
      TilingAlt.fromPolygon(4) // Central square (1,2,3,4)
        .addPolygon(Polygon(4), 1--2).getOrElse(fail()) // Top square
        .addPolygon(Polygon(4), 2--3).getOrElse(fail()) // Right square
        .addPolygon(Polygon(4), 3--4).getOrElse(fail()) // Bottom square
        .addPolygon(Polygon(4), 4--1).getOrElse(fail()) // Left square

  it should "return an error when trying to remove an internal polygon" in {
      val internalPolygon = Vector(1, 2, 3, 4).map(Node(_))
      val result = enclosedSquareTiling.removePolygon(internalPolygon)

      result.isLeft shouldBe true
      result.left.getOrElse(fail()) shouldBe "Polygon not sharing any edge with the perimeter. Cannot remove it."
    }

    // Tiling for testing disconnected edges: a square with two triangles on opposite sides
  val squareWithTwoTriangles: TilingAlt =
      TilingAlt.fromPolygon(4)
        .addPolygon(Polygon(3), 1--2).getOrElse(fail())
        .addPolygon(Polygon(3), 3--4).getOrElse(fail())

  it should "return an error if the polygon shares non-continuous edges with the perimeter" in {
      // The square (1,2,3,4) touches the perimeter at edges 2--3 and 4--1.
      // These two segments of the perimeter are separated by the triangles.
      val result = squareWithTwoTriangles.removePolygon(Vector(1, 2, 3, 4).map(Node(_)))
      result.isLeft shouldBe true
      result.left.getOrElse(fail()) shouldBe "Polygon sharing non-continuos edges with the perimeter. Cannot remove it."
  }
