package io.github.scala_tessella.tessella

import TilingGrowth.*
import TilingGrowth.OtherNodeStrategy.*
import TilingGrowth.PerimeterStrategy.*
import Outliers.*
import RegularPolygon.{Polygon, Vertex}
import Topology.{--, EdgeOrdering, Node}
import Geometry.Radian.{TAU_2, TAU_3, TAU_4, TAU_6}
import TilingSymmetry.countSymmetries
import Tiling.pattern_4444

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TilingGrowthSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A triangle" must "fail to have a node removed" in {
    triangle.maybeRemoveNode(Node(3)) shouldEqual
      Left(
        """Tiling must have each node connected to min 2 and max 6 other nodes, these nodes are not compliant: (1 [degree 1], 2 [degree 1]).
          |See DOT:
          |graph{
          |1 -- 2 [color=red]
          |1 [color=red fontcolor=red]
          |2 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  "A reticulate of triangles" can "have a perimeter node removed" in {
    p333333_4by4_reticulate.maybeRemoveNode(Node(1)).getOrElse(triangle).toString shouldBe
      "Tiling(2--3, 2--5, 2--6, 3--6, 4--5, 4--7, 4--8, 5--6, 5--8, 5--9, 6--9, 7--8, 7--10, 7--11, 8--9, 8--11, 8--12, 9--12, 10--11, 10--13, 10--14, 11--12, 11--14, 11--15, 12--15, 13--14, 14--15)"
  }

  it can "have an inner node removed" in {
    p333333_4by4_reticulate.maybeRemoveNode(Node(5)).isRight shouldBe
      true
  }

  it can "fail to have another perimeter node removed" in {
    p333333_4by4_reticulate.maybeRemoveNode(Node(2)).isRight shouldBe
      false
  }

  "A tiling made of six triangles" can "be grown by polygons added to the perimeter edges" in {
    val sixTriangles: Tiling =
      triangle.growByPolygon(5, Polygon(3), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL)).unsafe
    sixTriangles.graphEdges.sorted(EdgeOrdering).stringify shouldBe
      "1--2, 1--3, 1--4, 1--5, 1--6, 1--7, 2--3, 2--7, 3--4, 4--5, 5--6, 6--7"
  }

  "A tiling made of pentagons" can "be grown" in {
    pentagonGrown.countPolygons shouldBe
      10
  }

  "A tiling grown by polygons" can "find a dead end" in {
    val start: Tiling =
      Tiling.maybe(square.graphEdges ++ List(1--5, 2--5, 2--6, 3--6, 3--7, 4--7, 4--8, 1--8)).unsafe
    start.growByPolygon(1, Polygon(42), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
      .left.getOrElse("").take(81) shouldBe
      """Tiling cannot be grown after adding 0 * pgon-42,
        | no more edges fillable
        |See SVG:""".stripMargin     
  }

  "A tiling to be grown with a triangle plug" can "have a perimeter" in {
    minimalTriangleInsertion.perimeter shouldBe
      Vector(1, 7, 6, 5, 4, 3, 2).map(Node(_))
  }

  it can "have at node 1 inserted just a triangle" in {
    minimalTriangleInsertion.perimeterAngles shouldBe
      Map(
        5 -> TAU_3,
        1 -> TAU_6 * 5,
        6 -> TAU_3,
        2 -> TAU_6,
        7 -> TAU_6,
        3 -> TAU_3,
        4 -> TAU_3
      )
  }

  it can "have the triangle inserted growing node 1 starting from 2" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), FIXED(Node(2))).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the triangle inserted growing node 1 starting from 7" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), FIXED(Node(7))).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the triangle inserted growing node 7 starting from 1" in {
    minimalTriangleInsertion.maybeGrowNode(Node(7), Polygon(3), FIXED(Node(1))).isRight shouldBe
      true
  }

  it can "have the triangle inserted growing node 2 starting from 1" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), FIXED(Node(1))).isRight shouldBe
      true
  }

  "A tiling to be grown with a square plug" can "have a perimeter" in {
    minimalSquareInsertion.perimeter shouldBe
      Vector(1, 5, 9, 10, 11, 12, 8, 4, 3, 7, 6, 2).map(Node(_))
  }

  it can "have at nodes 6 or 7 inserted just a square" in {
    minimalSquareInsertion.perimeterAngles shouldBe
      Map(
        5 -> TAU_2,
        10 -> TAU_2,
        1 -> TAU_4,
        6 -> TAU_4 * 3,
        9 -> TAU_4,
        2 -> TAU_4,
        12 -> TAU_4,
        7 -> TAU_4 * 3,
        3 -> TAU_4,
        11 -> TAU_2,
        8 -> TAU_2,
        4 -> TAU_4
      )
  }

  it can "have the square inserted growing node 6 starting from 2" in {
    minimalSquareInsertion.maybeGrowNode(Node(6), Polygon(4), FIXED(Node(2))).unsafe.countSymmetries shouldBe
      2
  }

  it can "have the square inserted growing node 6 starting from 7" in {
    minimalSquareInsertion.maybeGrowNode(Node(6), Polygon(4), FIXED(Node(7))).unsafe.countSymmetries shouldBe
      2
  }

  it can "have the square inserted growing node 2 starting from 6" in {
    minimalSquareInsertion.maybeGrowNode(Node(2), Polygon(4), FIXED(Node(6))).isRight shouldBe
      true
  }

  it can "have the square inserted growing node 7 starting from 6" in {
    minimalSquareInsertion.maybeGrowNode(Node(7), Polygon(4), FIXED(Node(6))).unsafe.countSymmetries shouldBe
      2
  }

  it can "have the square inserted growing node 7 starting from 3" in {
    minimalSquareInsertion.maybeGrowNode(Node(7), Polygon(4), FIXED(Node(3))).unsafe.countSymmetries shouldBe
      2
  }

  it can "have the square inserted growing node 3 starting from 7" in {
    minimalSquareInsertion.maybeGrowNode(Node(3), Polygon(4), FIXED(Node(7))).isRight shouldBe
      true
  }

  "A triangle" can "be repeatedly grown on perimeter nodes" in {
    val grown =
      triangle.growByPolygon(23, Polygon(3), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
//    saveFile(
//      grown.unsafe.toSVG(fillPolygons = true, labelledNodes = LabelledNodes.ALL),
//      s"process/triangle"
//    )
    grown.unsafe.countSymmetries shouldBe
      6
  }

  it can "be repeatedly grown on perimeter edges" in {
    val grown =
      triangle.growByPolygonOnPerimeterEdges(23, Polygon(3), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
//    saveFile(
//      grown.unsafe.toSVG(fillPolygons = true, labelledNodes = LabelledNodes.ALL),
//      s"process/e_triangle"
//    )
    grown.unsafe.countSymmetries shouldBe
      6
  }

  "A square" can "be repeatedly grown" in {
    val grown =
      square.growByPolygon(24, Polygon(4), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
//    saveFile(
//      grown.unsafe.toSVG(fillPolygons = true, labelledNodes = LabelledNodes.ALL),
//      s"process/square"
//    )
    grown.unsafe.countSymmetries shouldBe
      4
  }

  "A hexagon" can "be repeatedly grown" in {
    val grown =
      hexagon.growByPolygon(18, Polygon(6), List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
//    saveFile(
//      grown.unsafe.toSVG(fillPolygons = true, labelledNodes = LabelledNodes.ALL),
//      s"process/hexagon"
//    )
    grown.unsafe.countSymmetries shouldBe
      6
  }

  "The growth by hexagon" can "be counted for polygons step by step" in {
    (0 until 10).map(p => hexagon.growByPolygon(
      p,
      Polygon(6),
      List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL)
    ).unsafe.countPolygons) shouldBe
      (1 to 10).toVector
  }

  "The growth by square" can "be counted for polygons step by step" in {
    (0 until 10).map(p => square.growByPolygon(
      p,
      Polygon(4),
      List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL)
    ).unsafe.countPolygons) shouldBe
      (1 to 10).toVector
  }

  "The growth by triangle" can "be counted for polygons step by step" in {
    (0 until 10).map(p => triangle.growByPolygon(
      p,
      Polygon(3),
      List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL)
    ).unsafe.countPolygons) shouldBe
      (1 to 10).toVector
  }

}
