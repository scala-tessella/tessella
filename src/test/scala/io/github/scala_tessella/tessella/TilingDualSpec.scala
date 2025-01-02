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

  val p31212reduced: Tiling =
    Tiling.maybe(p31212.graphEdges.filter(_.greaterNode.toInt <= 30)).unsafe

  val dualp31212reduced: TilingDual =
    p31212reduced.toTilingDual

  "A reduced 31212 pattern" can "be converted to dual" in {
    dualp31212reduced.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24) 1--29, 2--31, 3--31, 4--31, 5--31, 6--31, 7--31, 8--31, 9--30, 10--28, 11--28, 12--28, 13--28, 14--28, 15--28, 16--28, 17--27, 18--25, 19--25, 20--25, 21--25, 22--25, 23--25, 24--25, 25--26, 25--27, 25--28, 25--29, 25--31, 26--28, 26--31, 27--28, 28--30, 28--31, 29--31, 30--31)"
    dualp31212reduced.graphEdges.allDegrees.groupByValues shouldBe
      Map(
        1 -> List(5, 10, 14, 1, 6, 9, 13, 2, 17, 12, 7, 3, 18, 11, 8, 4, 15, 24, 20, 21, 22, 16, 23, 19),
        3 -> List(29, 27, 26, 30),
        12 -> List(25, 28, 31)
      ).mapValues2(_.map(Node(_)))
    dualp31212reduced.toDOT() shouldBe
      """graph{
        |1 -- 29
        |2 -- 31
        |3 -- 31
        |4 -- 31
        |5 -- 31
        |6 -- 31
        |7 -- 31
        |8 -- 31
        |9 -- 30
        |10 -- 28
        |11 -- 28
        |12 -- 28
        |13 -- 28
        |14 -- 28
        |15 -- 28
        |16 -- 28
        |17 -- 27
        |18 -- 25
        |19 -- 25
        |20 -- 25
        |21 -- 25
        |22 -- 25
        |23 -- 25
        |24 -- 25
        |25 -- 26
        |25 -- 31
        |25 -- 29
        |25 -- 27
        |25 -- 28
        |27 -- 28
        |28 -- 30
        |28 -- 31
        |26 -- 28
        |26 -- 31
        |30 -- 31
        |29 -- 31
        |}""".stripMargin
  }

  it can "be converted back" in {
    dualp31212reduced.toMaybeTiling shouldEqual
      Right(p31212reduced)
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

  val dualp33336: TilingDual =
    p33336.toTilingDual

  "A 33336 pattern" can "be converted to dual" in {
    //    dualp33336.toString shouldBe
    //      "TilingDual(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24) 1--29, 2--31, 3--31, 4--31, 5--31, 6--31, 7--31, 8--31, 9--30, 10--28, 11--28, 12--28, 13--28, 14--28, 15--28, 16--28, 17--27, 18--25, 19--25, 20--25, 21--25, 22--25, 23--25, 24--25, 25--26, 25--27, 25--28, 25--29, 25--31, 26--28, 26--31, 27--28, 28--30, 28--31, 29--31, 30--31)"
    dualp33336.graphEdges.allDegrees.groupByValues shouldBe
      Map(
        1 -> List(5, 10, 24, 14, 20, 1, 6, 21, 9, 13, 2, 17, 22, 12, 7, 3, 18, 16, 11, 23, 8, 19, 4, 15),
        3 -> List(69, 84, 65, 77, 66, 56, 42, 37, 25, 52, 46, 78, 29, 61, 74, 60, 85, 70, 28, 38, 53, 41, 73, 45, 32, 34, 44, 59, 27, 71, 54, 49, 81, 76, 39, 80, 35, 48, 63, 50, 67, 31, 43, 40, 26, 55, 75, 58, 82, 30, 51, 79, 68, 62),
        6 -> List(83, 57, 33, 64, 72, 36, 47)
      ).mapValues2(_.map(Node(_)))
    dualp33336.toDOT() shouldBe
      """graph{
        |1 -- 72
        |2 -- 73
        |3 -- 85
        |4 -- 84
        |5 -- 83
        |6 -- 82
        |7 -- 30
        |8 -- 31
        |9 -- 33
        |10 -- 26
        |11 -- 25
        |12 -- 28
        |13 -- 36
        |14 -- 35
        |15 -- 37
        |16 -- 46
        |17 -- 47
        |18 -- 48
        |19 -- 61
        |20 -- 63
        |21 -- 64
        |22 -- 65
        |23 -- 68
        |24 -- 71
        |69 -- 76
        |69 -- 70
        |58 -- 69
        |76 -- 83
        |76 -- 77
        |58 -- 59
        |57 -- 58
        |77 -- 78
        |57 -- 77
        |56 -- 57
        |43 -- 57
        |52 -- 57
        |55 -- 57
        |59 -- 72
        |59 -- 60
        |51 -- 56
        |56 -- 60
        |60 -- 62
        |70 -- 74
        |70 -- 72
        |66 -- 72
        |71 -- 72
        |72 -- 73
        |62 -- 64
        |62 -- 66
        |66 -- 67
        |74 -- 75
        |74 -- 83
        |79 -- 83
        |82 -- 83
        |83 -- 84
        |29 -- 78
        |78 -- 79
        |79 -- 80
        |54 -- 55
        |29 -- 55
        |29 -- 33
        |75 -- 85
        |73 -- 75
        |84 -- 85
        |51 -- 64
        |44 -- 51
        |42 -- 43
        |43 -- 44
        |44 -- 45
        |64 -- 65
        |63 -- 64
        |49 -- 64
        |67 -- 68
        |65 -- 67
        |68 -- 71
        |45 -- 47
        |45 -- 49
        |49 -- 50
        |52 -- 53
        |41 -- 52
        |42 -- 47
        |41 -- 42
        |40 -- 41
        |32 -- 54
        |53 -- 54
        |36 -- 53
        |47 -- 48
        |46 -- 47
        |39 -- 47
        |50 -- 61
        |48 -- 50
        |61 -- 63
        |36 -- 40
        |39 -- 40
        |38 -- 39
        |34 -- 36
        |28 -- 36
        |35 -- 36
        |32 -- 33
        |32 -- 34
        |27 -- 34
        |33 -- 80
        |31 -- 33
        |26 -- 33
        |80 -- 81
        |30 -- 81
        |81 -- 82
        |37 -- 38
        |35 -- 38
        |37 -- 46
        |25 -- 27
        |26 -- 27
        |25 -- 28
        |30 -- 31
        |}""".stripMargin
  }

  it can "be converted back" in {
    dualp33336.toMaybeTiling shouldEqual
      Right(p33336)
  }

  val dualp33434: TilingDual =
    p33434.toTilingDual

  "A 33434 pattern" can "be converted back from dual" in {
    dualp33434.toMaybeTiling shouldEqual
      Right(p33434)
  }

  val p33434reduced: Tiling =
    Tiling.pattern_33434(1).unsafe

  val dualp33434reduced: TilingDual =
    p33434reduced.toTilingDual

  "A 33434 reduced pattern" can "be converted back from dual" in {
    dualp33434reduced.toMaybeTiling shouldEqual
      Right(p33434reduced)
  }

  val tilings: List[Tiling] =
    List(
//      p31212, // issue
//      p666_4by4_reticulate, p666_grown_hexagon,
//      p3464,
//      p33336, // issue
//      p33434, // issue
//      p2x333333_2x33336_3366,
//      p2x333333_33336, p488,
//      p666_triangle, p3636, p4612
    )

  "Tilings" can "be converted back and forth" in {
    tilings.forall(t => Right(t) == t.toTilingDual.toMaybeTiling) shouldBe
      true
  }
}
