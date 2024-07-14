package io.github.scala_tessella.tessella

import Geometry.Radian.{TAU_3, TAU_6}
import Outliers.troubledGrowthByFullVertex
import RegularPolygon.{Polygon, Vertex}
import TilingGrowth.OtherNodeStrategy.*
import TilingSymmetry.countSymmetries
import Topology.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TilingGrowthNodePolygonSpec extends AnyFlatSpec with Helper with should.Matchers{

  val minimalTriangleInsertion: Tiling =
    Tiling.fromVertex(Vertex(3, 3, 3, 3, 3))

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

  it can "have a square growing node 6 starting from 5" in {
    minimalTriangleInsertion.maybeGrowNode(Node(6), Polygon(4), AFTER_PERIMETER).unsafe.graphEdges.size shouldBe
      14
  }

  it can "have the same with a fixed node 5 strategy" in {
    minimalTriangleInsertion.maybeGrowNode(Node(6), Polygon(4), FIXED(Node(5))).unsafe.graphEdges.size shouldBe
      14
  }

  it can "have the triangle inserted growing node 1 starting from 2" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), BEFORE_PERIMETER).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the same with a fixed node 2 strategy" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), FIXED(Node(2))).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the triangle inserted growing node 1 starting from 7" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), AFTER_PERIMETER).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the same with a fixed node 7 strategy" in {
    minimalTriangleInsertion.maybeGrowNode(Node(1), Polygon(3), FIXED(Node(7))).unsafe.countSymmetries shouldBe
      6
  }

  it can "have the triangle inserted growing node 7 starting from 1" in {
    minimalTriangleInsertion.maybeGrowNode(Node(7), Polygon(3), BEFORE_PERIMETER).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have the same with a fixed node 1 strategy" in {
    minimalTriangleInsertion.maybeGrowNode(Node(7), Polygon(3), FIXED(Node(1))).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have the triangle inserted growing node 2 starting from 1" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), AFTER_PERIMETER).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have the same result with a fixed node 1 strategy" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), FIXED(Node(1))).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have the triangle inserted growing node 2 starting from the lower ordinal" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), LOWER_ORDINAL).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have the triangle inserted growing node 2 starting from a non-existing perimeter node" in {
    (
      minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), FIXED(Node(100))) match
        case Left(msg) => msg.contains("Tiling can add polygons only to perimeter nodes:\n found unknown node 100")
        case Right(_)  => false
      ) shouldBe
      true
  }

  it can "have the triangle inserted growing node 2 starting from the higher ordinal" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), HIGHER_ORDINAL).unsafe.graphEdges.size shouldBe
      13
  }

  it can "have the triangle inserted growing node 2 starting from the wider external angle" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), WIDER_ANGLE, BEFORE_PERIMETER).unsafe.graphEdges.size shouldBe
      13
  }

  it can "have the triangle inserted growing node 2 starting from the narrower external angle" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Polygon(3), NARROWER_ANGLE, BEFORE_PERIMETER).unsafe.graphEdges.size shouldBe
      12
  }

  it can "have a square growing node 5 starting from the narrower external angle and then lower ordinal" in {
    minimalTriangleInsertion.maybeGrowNode(Node(5), Polygon(4), WIDER_ANGLE, LOWER_ORDINAL).unsafe.countSymmetries shouldBe
      1
  }

  it can "have a square growing node 5 starting from the narrower external angle and then higher ordinal" in {
    minimalTriangleInsertion.maybeGrowNode(Node(5), Polygon(4), WIDER_ANGLE, HIGHER_ORDINAL).unsafe.countSymmetries shouldBe
      0
  }

  it can "have a vertex inserted growing node 2 starting from 1" in {
    minimalTriangleInsertion.maybeGrowNode(Node(2), Vertex(Vector(3, 4).map(Polygon(_))), AFTER_PERIMETER).unsafe.graphEdges.size shouldBe
      15
  }

  it can "have a square growing on edge starting from the first edge's node on the perimeter" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), BEFORE_PERIMETER).unsafe.graphEdges.contains(5--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from the second edge's node on the perimeter" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), AFTER_PERIMETER).unsafe.graphEdges.contains(4--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from one node" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), FIXED(Node(4))).unsafe.graphEdges.contains(4--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from the other node" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), FIXED(Node(5))).unsafe.graphEdges.contains(5--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from an incorrect node, falls back to perimeter direction" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), FIXED(Node(100))).unsafe.graphEdges.contains(5--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from the lower edge's node" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), LOWER_ORDINAL).unsafe.graphEdges.contains(4--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from the higher edge's node" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), HIGHER_ORDINAL).unsafe.graphEdges.contains(5--8) shouldBe
      true
  }

  it can "have a square growing on edge starting from the wider angle edge's node and then fallback to higher ordinal" in {
    minimalTriangleInsertion.maybeGrowEdge(4--5, Polygon(4), WIDER_ANGLE, HIGHER_ORDINAL).unsafe.graphEdges.contains(5--8) shouldBe
      true
  }

  it can "have a square growing on another edge starting from the narrower angle edge's node and then fallback to higher ordinal" in {
    minimalTriangleInsertion.maybeGrowEdge(6--7, Polygon(4), NARROWER_ANGLE, HIGHER_ORDINAL).unsafe.graphEdges.contains(6--8) shouldBe
      true
  }

  it can "have a square growing on another edge starting from the wider angle edge's node and then fallback to higher ordinal" in {
    minimalTriangleInsertion.maybeGrowEdge(6--7, Polygon(4), WIDER_ANGLE, HIGHER_ORDINAL).unsafe.graphEdges.contains(7--8) shouldBe
      true
  }


  val vertex33333: Vertex =
    Vertex(3, 3, 3, 3, 3)

  "A troubled tiling" can "have a filling vertex growing node 4 starting from 1" in {
    troubledGrowthByFullVertex.maybeGrowNode(Node(4), vertex33333, AFTER_PERIMETER).isRight shouldBe
      true
  }

  it can "have a filling vertex growing node 4 starting from 7" in {
    troubledGrowthByFullVertex.maybeGrowNode(Node(4), vertex33333, BEFORE_PERIMETER).isRight shouldBe
      true
  }

  val vertex3333: Vertex =
    Vertex(3, 3, 3, 3)

  it can "NOT have a vertex growing node 4 starting from 1 to touch one perimeter node" in {
    (
      troubledGrowthByFullVertex.maybeGrowNode(Node(4), vertex3333, AFTER_PERIMETER) match
        case Left(msg) => msg.contains("Tiling must have all perimeter nodes at different cartesian coords:\n found invalid couple (7,15)")
        case Right(_)  => false
    ) shouldBe
      true
  }

  it can "NOT have a vertex growing node 4 starting from 7 to touch one perimeter node" in {
    (
      troubledGrowthByFullVertex.maybeGrowNode(Node(4), vertex3333, BEFORE_PERIMETER) match
        case Left(msg) => msg.contains("Tiling must have all perimeter nodes at different cartesian coords:\n found invalid couple (1,15)")
        case Right(_)  => false
    ) shouldBe
      true
  }

  val intersectTestbed: Tiling =
    Tiling.maybe(Tiling.pattern_4444(3, 3).unsafe.graphEdges.diff(List(2--3, 6--7))).unsafe

  it can "NOT have a vertex growing node 3 starting from 7 to intersect some perimeter edges" in {
    (
      intersectTestbed.maybeGrowNode(Node(3), Polygon(6), AFTER_PERIMETER) match
        case Left(msg) => msg.contains("Tiling must not have intersecting perimeter edges:\n found invalid couples ((6--10, 17--18), (1--2, 18--19))")
        case Right(_)  => false
    ) shouldBe
      true
  }

  val anotherIntersectTestbed: Tiling =
    Tiling.maybe(
      2--8, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 1--7, 1--5, 1--4, 1--3, 1--2, 3--9, 9--10, 2--10, 4--11, 3--11, 9--12,
      11--12, 5--13, 6--13, 10--14, 9--14, 12--15, 9--15, 14--15, 10--16, 14--16, 15--17, 17--18, 14--18, 16--19,
      18--19, 15--20, 17--20, 12--21, 20--21, 18--22, 17--22, 7--23, 6--23, 8--24, 7--24, 23--24, 4--25, 25--26, 11--26,
      19--27, 18--27, 22--27, 8--28, 28--29, 24--29, 23--30, 30--31, 24--31, 29--31, 19--32, 27--32, 22--33, 33--34,
      27--34, 32--35, 34--35, 28--36, 36--37, 29--37, 31--38, 29--38, 37--38, 23--39, 39--40, 30--40, 31--41, 30--41,
      40--42, 30--42, 41--42, 38--43, 41--43, 22--44, 33--44, 34--45, 33--45, 35--46, 34--46, 45--46, 28--47, 36--47,
      39--48, 40--48, 42--49, 49--50, 41--50, 43--50, 35--51, 46--51, 45--52, 52--53, 46--53, 51--54, 53--54, 28--55,
      47--55, 36--56, 47--56, 55--57, 57--58, 47--58, 56--59, 58--59, 39--60, 48--60, 42--61, 61--62, 49--62, 50--63,
      49--63, 62--64, 49--64, 63--64, 45--65, 52--65, 53--66, 52--66, 54--67, 53--67, 66--67
    ).unsafe

  "Another tiling" can "NOT have triangle grown on edge 55--57" in {
    Tiling.maybe(anotherIntersectTestbed.graphEdges ++ List(55--68, 57--68)).isRight shouldBe
      false
  }

  it can "NOT have a vertex growing node 14 starting from 15 to intersect some perimeter edges" in {
    val grown: Either[String, Tiling] =
      anotherIntersectTestbed.maybeGrowNode(Node(55), Polygon(3), FIXED(Node(57)))
    (
      grown match
        case Left(msg) => msg.contains("Tiling must not have intersecting perimeter edges:\n found invalid couples ((2--10, 55--68), (2--10, 57--68))")
        case Right(_) => false
      ) shouldBe
      true
  }

  val touchingTestbed: Tiling =
    Tiling.maybe(
      2--14, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 11--12, 12--13, 13--14, 1--5, 1--3, 1--2, 5--15, 4--15,
      4--16, 16--17, 15--17, 8--18, 7--18
    ).unsafe

  "A polygon touching the perimeter" can "NOT be added" in {
    touchingTestbed.maybeGrowNode(Node(7), Polygon(12), FIXED(Node(18))).isRight shouldBe
      false
  }

  val edges12Nodes8: Tiling =
    Tiling.maybe(1--2, 1--3, 2--3, 2--4, 4--5, 5--3, 4--7, 7--5, 4--6, 7--6, 8--6, 8--7).unsafe

  val vertex: Vertex =
    Vertex(3, 3, 4, 4)

  "A tiling" can "have a vertex entirely filling a perimeter node" in {
    val r: Tiling =
      edges12Nodes8.maybeGrowNode(Node(1), vertex, AFTER_PERIMETER).unsafe
    r.nonPerimeterOrderedPolygons(Node(1)) shouldBe
      Vector(4, 4, 3, 3, 3)
  }

  it can "have a vertex entirely filling a perimeter node starting from a node" in {
    val r: Tiling =
      edges12Nodes8.maybeGrowNode(Node(1), vertex, FIXED(Node(3))).unsafe
    r.nonPerimeterOrderedPolygons(Node(1)) shouldBe
      Vector(3, 3, 4, 4, 3)
  }

  val vertexPartial: Vertex =
    Vertex(3, 3, 4)

  val r1: Tiling =
    edges12Nodes8.maybeGrowNode(Node(1), vertexPartial, BEFORE_PERIMETER).unsafe

  val rEdges: List[Edge] =
    List(
      1--2, 1--3, 2--3, 2--4, 4--5, 3--5, 4--7, 5--7, 4--6, 6--7,
      6--8, 7--8, 3--9, 1--9, 9--10, 1--10, 10--11, 11--12, 1--12
    )

  "A edges12Nodes8" can "have a partial vertex grown around perimeter node 1 starting from" in {
    r1.perimeterOrderedPolygons(Node(1)) shouldBe
      Vector(4, 3, 3, 3)
  }

  it can "return all edges" in {
    r1.graphEdges shouldBe rEdges
  }

  val r2: Tiling =
    edges12Nodes8.maybeGrowNode(Node(1), vertexPartial, FIXED(Node(3))).unsafe

  it can "have a partial vertex grown around perimeter node 1 starting from 3" in {
    r2.perimeterOrderedPolygons(Node(1)) shouldBe
      Vector(4, 3, 3, 3)
  }

  it can "return the same all edges" in {
    r2.graphEdges shouldBe rEdges
  }

}
