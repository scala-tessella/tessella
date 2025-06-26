package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.Geometry.Point
import io.github.scala_tessella.tessella.Topology.Node
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IncrementalTilingValidationSpec extends AnyFlatSpec with Matchers {

  "The withValidation method" should "create a valid tiling from consistent data" in {
    val square = IncrementalTiling.fromPolygon(4)
    val result = IncrementalTiling.withValidation(square.orientedPolygons, square.perimeter, square.coordinates)
    result.isRight shouldBe true
    result.getOrElse(fail("Expected a valid Tiling")) shouldBe square
  }

  it should "create a valid empty tiling" in {
    val result = IncrementalTiling.withValidation(Nil, Vector.empty, Map.empty)
    result.isRight shouldBe true
    result.getOrElse(fail("Expected an empty Tiling")) shouldBe IncrementalTiling.empty
  }

  it should "return an error for an empty tiling with a non-empty perimeter" in {
    val result = IncrementalTiling.withValidation(Nil, Vector(Node(1)), Map.empty)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "For empty polygons, perimeter and coordinates must also be empty."
  }

  it should "return an error for an empty tiling with non-empty coordinates" in {
    val result = IncrementalTiling.withValidation(Nil, Vector.empty, Map(Node(1) -> Point(0, 0)))
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "For empty polygons, perimeter and coordinates must also be empty."
  }

  it should "return an error for inconsistent perimeter" in {
    val square = IncrementalTiling.fromPolygon(4)
    val invalidPerimeter = square.perimeter.tail // Perimeter with a missing edge
    val result = IncrementalTiling.withValidation(square.orientedPolygons, invalidPerimeter, square.coordinates)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) should startWith("Perimeter edges are inconsistent.")
  }

  it should "return an error for a disconnected tiling" in {
    val square1Poly = Vector(1, 2, 3, 4).map(Node.apply)
    val square2Poly = Vector(5, 6, 7, 8).map(Node.apply)
    val polys = List(square1Poly, square2Poly)
    val square1Coords = Map(
      Node(1) -> Point(0, 0), Node(2) -> Point(0, 1), Node(3) -> Point(1, 1), Node(4) -> Point(1, 0)
    )
    val square2Coords = Map(
      Node(5) -> Point(2, 0), Node(6) -> Point(2, 1), Node(7) -> Point(3, 1), Node(8) -> Point(3, 0)
    )
    val coords = square1Coords ++ square2Coords
    // For a disconnected tiling, the perimeter cannot be represented by a single vector of nodes,
    // which will cause the perimeter validation to fail before the connectivity check.
    // Here, we supply a perimeter that is also invalid to demonstrate failure.
    val perimeter = square1Poly ++ square2Poly
    val result = IncrementalTiling.withValidation(polys, perimeter, coords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) should startWith("Perimeter edges are inconsistent.")
  }

  it should "return an error for missing node coordinates" in {
    val square = IncrementalTiling.fromPolygon(4)
    val incompleteCoords = square.coordinates - Node(4)
    val result = IncrementalTiling.withValidation(square.orientedPolygons, square.perimeter, incompleteCoords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Mismatch between graph nodes and coordinate nodes. Missing: Set(4), Extra: Set()"
  }

  it should "return an error for extra node coordinates" in {
    val square = IncrementalTiling.fromPolygon(4)
    val extraCoords = square.coordinates + (Node(5) -> Point(2, 2))
    val result = IncrementalTiling.withValidation(square.orientedPolygons, square.perimeter, extraCoords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Mismatch between graph nodes and coordinate nodes. Missing: Set(), Extra: Set(5)"
  }

  it should "return an error for incorrect edge lengths" in {
    val square = IncrementalTiling.fromPolygon(4)
    val wrongLengthCoords = square.coordinates.updated(Node(3), Point(1.0, 2.0)) // Edge 2--3 will have length > 1
    val result = IncrementalTiling.withValidation(square.orientedPolygons, square.perimeter, wrongLengthCoords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Edge 2--3 has length not equal to 1."
  }

  it should "return an error for overlapping nodes" in {
    val square = IncrementalTiling.fromPolygon(4)
    // Move Node(3) to the coordinates of Node(1), creating overlapping nodes.
    val overlappingCoords = square.coordinates.updated(Node(3), square.coordinates(Node(1)))
    // This setup serendipitously maintains edge lengths of 1 for the square's edges.
    // It will pass the edge length check but fail on the overlapping nodes check.
    val result = IncrementalTiling.withValidation(square.orientedPolygons, square.perimeter, overlappingCoords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Found distinct nodes with almost identical coordinates."
  }

  it should "return an error for incorrect interior angles" in {
    val rhombusPoly = Vector(1, 2, 3, 4).map(Node.apply)
    val rhombusPerimeter = rhombusPoly
    val rhombusCoords = Map(
      Node(1) -> Point(0.0, 0.0),
      Node(2) -> Point(1.0, 0.0),
      Node(3) -> Point(1.5, math.sqrt(3.0) / 2.0),
      Node(4) -> Point(0.5, math.sqrt(3.0) / 2.0)
    )
    // All edge lengths are 1.0, but the interior angles are 60 and 120 degrees.
    // The validation expects 90-degree angles for a 4-sided polygon.
    val result = IncrementalTiling.withValidation(List(rhombusPoly), rhombusPerimeter, rhombusCoords)
    result.isLeft shouldBe true
    result.left.getOrElse(fail()) shouldBe "Invalid interior angle for polygon 1,2,3,4 at node 1."
  }

}
