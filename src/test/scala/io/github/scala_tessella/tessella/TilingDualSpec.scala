package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.{--, Degree, Edge, Node}
import TopologyDual.*
import utility.Utils.mapKeys

import io.github.scala_tessella.ring_seq.RingSeq.isRotationOrReflectionOf
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingDualSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A triangle" can "be converted" in {
    triangle.toTilingDual.toString shouldBe
      "TilingDual(Vector(1, 2, 3) 1--4, 2--4, 3--4)"
    triangle.toTilingDual.polygonBoundary shouldBe
      Vector(3, 3, 3)
  }

  "An hexagon made of triangles" can "be converted" in {
    hexagonTriangles.toTilingDual.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6) 1--9, 2--10, 3--12, 4--11, 5--7, 6--8, 7--8, 7--11, 8--9, 9--10, 10--12, 11--12)"
    hexagonTriangles.toTilingDual.polygonBoundary shouldBe
      Vector(3, 3, 3, 3, 3, 3)
  }

  val triangleAndSquareEdges: List[Edge] =
    List(1--2, 1--3, 2--3, 2--4, 3--5, 4--5)

  val squareWithTwoOppositeTriangles: Tiling =
    Tiling.maybe(triangleAndSquareEdges ++ List(4--6, 5--6)).unsafe

  val squareWithTwoAdjacentTriangles: Tiling =
    Tiling.maybe(triangleAndSquareEdges ++ List(2--6, 4--6)).unsafe

  "A tiling made of a square with two triangles" must "be different if the triangles are opposite or adjacent" in {
    squareWithTwoOppositeTriangles == squareWithTwoAdjacentTriangles shouldBe
      false
  }

  val dualSquareWithTwoOppositeTriangles: TilingDual =
    squareWithTwoOppositeTriangles.toTilingDual

  val dualSquareWithTwoAdjacentTriangles: TilingDual =
    squareWithTwoAdjacentTriangles.toTilingDual

  it can "NOT be distinguished based purely on the graph dual" in {
    val oppositeDegrees: Map[Node, Degree] =
      dualSquareWithTwoOppositeTriangles.graphEdges.allDegrees
    val adjacentDegrees: Map[Node, Degree] =
      dualSquareWithTwoAdjacentTriangles.graphEdges.allDegrees
    oppositeDegrees shouldEqual
      adjacentDegrees
    oppositeDegrees shouldBe
      Map(
        1 -> 1,
        2 -> 1,
        3 -> 1,
        4 -> 1,
        5 -> 1,
        6 -> 1,
        7 -> 3,
        8 -> 4,
        9 -> 3
      ).mapKeys(Node(_))
  }

  it must "be distinguished based on the ordered boundary polygons" in {
    val oppositePolygonBoundary: Vector[Polygon] =
      dualSquareWithTwoOppositeTriangles.polygonBoundary
    val adjacentPolygonBoundary: Vector[Polygon] =
      dualSquareWithTwoAdjacentTriangles.polygonBoundary
    oppositePolygonBoundary.isRotationOrReflectionOf(adjacentPolygonBoundary) shouldBe
        false
    oppositePolygonBoundary shouldBe
      Vector(3, 4, 3, 3, 4, 3)
    adjacentPolygonBoundary shouldBe
      Vector(3, 3, 3, 4, 4, 3)
  }

  "A tiling made of a square with two opposite triangles" can "be printed" in {
    dualSquareWithTwoOppositeTriangles.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6) 1--9, 2--8, 3--7, 4--7, 5--8, 6--9, 7--8, 8--9)"
  }

  "A tiling made of a square with two adjacent triangles" can "be printed" in {
    dualSquareWithTwoAdjacentTriangles.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6) 1--9, 2--7, 3--7, 4--8, 5--8, 6--9, 7--8, 8--9)"
  }
  
  it can "be converted to a Tiling" in {
    dualSquareWithTwoAdjacentTriangles.toMaybeTiling shouldEqual
      Right(squareWithTwoAdjacentTriangles)
  }

}
