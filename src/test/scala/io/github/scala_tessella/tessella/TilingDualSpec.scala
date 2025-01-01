package io.github.scala_tessella.tessella

import Outliers.*
import RegularPolygon.Polygon
import Topology.{--, Degree, Edge, Node}
import TopologyDual.*
import conversion.DOT.toDOT
import utility.Utils.*

import io.github.scala_tessella.ring_seq.RingSeq.isRotationOrReflectionOf
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingDualSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A triangle" can "be converted to its dual" in {
    triangle.toTilingDual.toString shouldBe
      "TilingDual(Vector(1, 2, 3) 1--4, 2--4, 3--4)"
    triangle.toTilingDual.polygonBoundary shouldBe
      Vector(3, 3, 3)
  }

  "An hexagon made of triangles" can "be converted to its dual" in {
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

  it can "be converted back to a Tiling" in {
    dualSquareWithTwoOppositeTriangles.toMaybeTiling shouldEqual
      Right(squareWithTwoOppositeTriangles)
  }

  "A tiling made of a square with two adjacent triangles" can "be printed" in {
    dualSquareWithTwoAdjacentTriangles.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6) 1--9, 2--7, 3--7, 4--8, 5--8, 6--9, 7--8, 8--9)"
  }

  it can "be converted back to a Tiling" in {
    dualSquareWithTwoAdjacentTriangles.toMaybeTiling shouldEqual
      Right(squareWithTwoAdjacentTriangles)
  }

  val fourSquares: Tiling =
    Tiling.pattern_4444(2, 2).unsafe

  val dualFourSquares: TilingDual =
    fourSquares.toTilingDual

  "A tiling made of four squares" can "be converted to its dual" in {
    dualFourSquares.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6, 7, 8) 1--12, 2--9, 3--9, 4--10, 5--10, 6--11, 7--11, 8--12, 9--10, 9--12, 10--11, 11--12)"
    dualFourSquares.polygonBoundary shouldBe
      Vector(4, 4, 4, 4, 4, 4, 4, 4)
  }

  it can "be converted back to a Tiling" in {
    dualFourSquares.toMaybeTiling shouldEqual
      Right(fourSquares)
  }

  val dualp31212: TilingDual =
    p31212.toTilingDual

  "A 31212 pattern" can "be converted to dual" in {
    dualp31212.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36) 1--40, 2--40, 3--40, 4--40, 5--40, 6--42, 7--43, 8--43, 9--43, 10--43, 11--43, 12--44, 13--46, 14--46, 15--46, 16--46, 17--46, 18--47, 19--49, 20--49, 21--49, 22--49, 23--49, 24--50, 25--52, 26--52, 27--52, 28--52, 29--52, 30--53, 31--55, 32--55, 33--55, 34--55, 35--55, 36--54, 37--38, 37--39, 37--40, 37--41, 37--43, 37--45, 37--46, 37--48, 37--49, 37--51, 37--52, 37--55, 38--40, 38--43, 39--40, 39--55, 40--42, 40--43, 40--54, 40--55, 41--43, 41--46, 42--43, 43--44, 43--46, 44--46, 45--46, 45--49, 46--47, 46--49, 47--49, 48--49, 48--52, 49--50, 49--52, 50--52, 51--52, 51--55, 52--53, 52--55, 53--55, 54--55)"
    dualp31212.graphEdges.allDegrees.groupByValues shouldBe
      Map(
        1 -> List(5, 10, 1, 6, 2, 8, 4, 24, 25, 14, 20, 29, 21, 33, 28, 9, 13, 17, 32, 34, 22, 27, 12, 7, 3, 35, 18, 16, 31, 11, 26, 23, 36, 30, 19, 15),
        3 -> List(42, 38, 53, 41, 45, 44, 54, 39, 48, 50, 51, 47),
        12 -> List(37, 52, 46, 49, 43, 40, 55)
      ).mapValues2(_.map(Node(_)))
    dualp31212.toDOT() shouldBe
      """graph{
        |1 -- 40
        |2 -- 40
        |3 -- 40
        |4 -- 40
        |5 -- 40
        |6 -- 42
        |7 -- 43
        |8 -- 43
        |9 -- 43
        |10 -- 43
        |11 -- 43
        |12 -- 44
        |13 -- 46
        |14 -- 46
        |15 -- 46
        |16 -- 46
        |17 -- 46
        |18 -- 47
        |19 -- 49
        |20 -- 49
        |21 -- 49
        |22 -- 49
        |23 -- 49
        |24 -- 50
        |25 -- 52
        |26 -- 52
        |27 -- 52
        |28 -- 52
        |29 -- 52
        |30 -- 53
        |31 -- 55
        |32 -- 55
        |33 -- 55
        |34 -- 55
        |35 -- 55
        |36 -- 54
        |37 -- 38
        |37 -- 43
        |37 -- 41
        |37 -- 46
        |37 -- 45
        |37 -- 49
        |37 -- 48
        |37 -- 52
        |37 -- 51
        |37 -- 55
        |37 -- 39
        |37 -- 40
        |39 -- 40
        |40 -- 55
        |40 -- 54
        |40 -- 42
        |40 -- 43
        |38 -- 40
        |38 -- 43
        |39 -- 55
        |42 -- 43
        |43 -- 44
        |43 -- 46
        |41 -- 43
        |41 -- 46
        |44 -- 46
        |46 -- 47
        |46 -- 49
        |45 -- 46
        |45 -- 49
        |47 -- 49
        |49 -- 50
        |49 -- 52
        |48 -- 49
        |48 -- 52
        |50 -- 52
        |52 -- 53
        |52 -- 55
        |51 -- 52
        |51 -- 55
        |53 -- 55
        |54 -- 55
        |}""".stripMargin
  }

  it can "be converted back" in {
    dualp31212.toMaybeTiling shouldEqual
      Right(p31212)
  }

//  val tilings: List[Tiling] =
//    List(
////      p31212,
////      p3464, p33336,
////      p33434, p2x333333_2x33336_3366,
////      p2x333333_33336, p488,
////      p666_4by4_reticulate, p666_grown_hexagon,
////      p666_triangle, p3636, p4612
//    )
//
//  "Tilings" can "be converted back and forth" in {
//    tilings.forall(t => Right(t) == t.toTilingDual.toMaybeTiling) shouldBe
//      true
//  }
}
