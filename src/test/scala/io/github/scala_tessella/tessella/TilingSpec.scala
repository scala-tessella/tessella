package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.{TAU_2, TAU_3, TAU_4, TAU_6}
import Outliers.{p4444_4by4_reticulate, *}
import RegularPolygon.{Polygon, Vertex}
import TilingSymmetry.{countRotationalSymmetries, countSymmetries}
import Topology.*
import utility.Utils.{mapKeys, mapValues2}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TilingSpec extends AnyFlatSpec with Accuracy with should.Matchers {

  val path1: p4444_4by4_reticulate.Path =
    p4444_4by4_reticulate.Path(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25).map(Node(_)))

  val path2: p4444_4by4_reticulate.Path =
    p4444_4by4_reticulate.Path(Vector(1, 2, 7, 8).map(Node(_)))

  val ringPath1: p4444_4by4_reticulate.PolygonPath =
    p4444_4by4_reticulate.PolygonPath(Vector(1, 2, 7, 6).map(Node(_)))

  val ringPath2: p4444_4by4_reticulate.PolygonPath =
    p4444_4by4_reticulate.PolygonPath(Vector(3, 2, 7, 8).map(Node(_)))

  val ringPath3: p4444_4by4_reticulate.PolygonPath =
    p4444_4by4_reticulate.PolygonPath(Vector(6, 7, 12, 11).map(Node(_)))

  val ringPath4: p4444_4by4_reticulate.PolygonPath =
    p4444_4by4_reticulate.PolygonPath(Vector(8, 7, 12, 13).map(Node(_)))

  val full: List[p4444_4by4_reticulate.PolygonPath] =
    List(ringPath1, ringPath2, ringPath3, ringPath4)

  "A tiling" must "have a perimeter as a circular path" in {
    p4444_4by4_reticulate.perimeter shouldEqual
      Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 24, 23, 22, 21, 16, 11, 6)
  }

  it must "have a map of perimeter polygons as circular paths" in {
    p4444_4by4_reticulate.perimeterOrderedPolygonPaths shouldEqual
      Map(
        5 -> Vector(Vector(5, 10, 9, 4)),
        10 -> Vector(Vector(10, 15, 14, 9), Vector(10, 9, 4, 5)),
        20 -> Vector(Vector(20, 25, 24, 19), Vector(20, 19, 14, 15)),
        1 -> Vector(Vector(1, 2, 7, 6)),
        6 -> Vector(Vector(6, 1, 2, 7), Vector(6, 7, 12, 11)),
        21 -> Vector(Vector(21, 16, 17, 22)),
        2 -> Vector(Vector(2, 3, 8, 7), Vector(2, 7, 6, 1)),
        22 -> Vector(Vector(22, 21, 16, 17), Vector(22, 17, 18, 23)),
        3 -> Vector(Vector(3, 4, 9, 8), Vector(3, 8, 7, 2)),
        16 -> Vector(Vector(16, 11, 12, 17), Vector(16, 17, 22, 21)),
        11 -> Vector(Vector(11, 6, 7, 12), Vector(11, 12, 17, 16)),
        23 -> Vector(Vector(23, 22, 17, 18), Vector(23, 18, 19, 24)),
        4 -> Vector(Vector(4, 5, 10, 9), Vector(4, 9, 8, 3)),
        15 -> Vector(Vector(15, 20, 19, 14), Vector(15, 14, 9, 10)),
        24 -> Vector(Vector(24, 23, 18, 19), Vector(24, 19, 20, 25)),
        25 -> Vector(Vector(25, 24, 19, 20))
      )
  }

  "A tiling" can "be printed" in {
    triangle.toString shouldBe
      "Tiling(1--2, 1--3, 2--3)"
  }

  it can "be a single pentagon with 5 nodes and 5 edges" in {
    (pentagon.graphNodes.size, pentagon.graphEdges.size) shouldBe
      (5, 5)
  }

  it can "NOT be publicly created with a constructor" in {
    "Tiling(triangle.edges)" shouldNot
      compile
  }

  it can " be publicly created with validation" in {
    "Tiling.maybe(triangle.graphEdges)" should
      compile
  }

  it must "be publicly created as an Either" in {
    Tiling.maybe(triangle.graphEdges).isRight shouldBe
      true
  }

  it can "be empty" in {
    Tiling.maybe(Nil).isRight shouldBe
      true
  }

  it can "have an empty parameter" in {
    Tiling.empty.perimeter shouldBe
      Vector.empty
  }

  it can "have  empty spatial coordinates" in {
    Tiling.empty.coords shouldBe
      Map()
  }

  it must "discard duplicated edges" in {
    Tiling.maybe(triangle.graphEdges ++ triangle.graphEdges) shouldEqual
      Right(triangle)
  }

  it must "have a perimeter" in {
    triangle.perimeter.toRingEdges.toList.sorted(EdgeOrdering) shouldEqual
      triangle.graphEdges.sorted(EdgeOrdering)
  }

  "A triangle" can "have perimeter nodes and perimeter path angles" in {
    (
      triangle.perimeter.toRingNodes,
      triangle.orderedPerimeterAngles,
    ) shouldBe
      (Vector(1, 3, 2), Vector(TAU_6, TAU_6, TAU_6))
  }

  it can "have perimeter length, area and compactness measured" in {
    (
      triangle.perimeterLength,
      triangle.area,
      triangle.compactness
    ) shouldBe
      (3, 0.43301270189221946, 0.6045997880780728)
  }

  it can "transform perimeter into cartesian points" in {
    val expected: Vector[Point] =
      Vector(Point(), Point(0.5, S6), Point(1, 0))
    triangle.perimeterPoints.almostEquals(expected) shouldBe
      true
  }

  it can "have a map of nodes and cartesian points" in {
    triangle.perimeterCoords.almostEqualsMap(Map(
      1 -> Point(),
      3 -> Point(0.5, S6),
      2 -> Point(1, 0)
    ).mapKeys(Node(_))) shouldBe
      true
  }

  "A sqr4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      p4444_4by4_reticulate.perimeter.toRingNodes,
      p4444_4by4_reticulate.orderedPerimeterAngles,
    ) shouldBe
      (
        Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 24, 23, 22, 21, 16, 11, 6),
        Vector(
          TAU_4, TAU_2, TAU_2, TAU_2, TAU_4, TAU_2, TAU_2, TAU_2,
          TAU_4, TAU_2, TAU_2, TAU_2, TAU_4, TAU_2, TAU_2, TAU_2
        )
      )
  }

  it must "have perimeter length, area and compactness measured" in {
    (
      p4444_4by4_reticulate.perimeterLength,
      p4444_4by4_reticulate.area,
      p4444_4by4_reticulate.compactness
    ) shouldBe
      (16, 16.000000000000004, 0.7853981633974484)
  }

//  it must "have an area calculated with an alternative method" in {
//    (sqr4x4Reticulate.areaAlt ~= 16) shouldBe
//      true
//  }

  it can "transform perimeter into cartesian points" in {
    val expected: Vector[Point] =
      Vector(
        Point(), Point(0, 1), Point(0, 2), Point(0, 3), Point(0, 4), Point(1, 4), Point(2, 4),
        Point(3, 4), Point(4, 4), Point(4, 3), Point(4, 2), Point(4, 1), Point(4, 0), Point(3, 0),
        Point(2, 0), Point(1, 0)
      )
    p4444_4by4_reticulate.perimeterPoints.almostEquals(expected) shouldBe
      true
  }

  it must "have a gonality value" in {
    p4444_4by4_reticulate.gonality shouldBe
      1
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (p4444_4by4_reticulate.countSymmetries, p4444_4by4_reticulate.countRotationalSymmetries) shouldBe
      (4, 4)
  }

  "A tri4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      p333333_4by4_reticulate.perimeter.toRingNodes,
      p333333_4by4_reticulate.orderedPerimeterAngles,
    ) shouldBe
      (
        Vector(1, 2, 3, 6, 9, 12, 15, 14, 13, 10, 7, 4),
        Vector(TAU_3, TAU_2, TAU_6, TAU_2, TAU_2, TAU_2, TAU_3, TAU_2, TAU_6, TAU_2, TAU_2, TAU_2)
      )
  }

  it must "have perimeter length, area and compactness measured" in {
    (
      p333333_4by4_reticulate.perimeterLength,
      p333333_4by4_reticulate.area,
      p333333_4by4_reticulate.compactness
    ) shouldBe
      (12, 6.928203230275511, 0.6045997880780728)
  }

  it must "have a gonality value" in {
    p333333_4by4_reticulate.gonality shouldBe
      1
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (p333333_4by4_reticulate.countSymmetries, p333333_4by4_reticulate.countRotationalSymmetries) shouldBe
      (0, 2)
  }

  "A hex4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      p666_4by4_reticulate.perimeter.toRingNodes,
      p666_4by4_reticulate.orderedPerimeterAngles,
    ) shouldBe
      (
        Vector(
          1, 10, 11, 20, 21, 30, 31, 40, 41, 42, 43, 44, 45, 46, 47,
          48, 39, 38, 29, 28, 19, 18, 9, 8, 7, 6, 5, 4, 3, 2
        ),
        Vector(
          TAU_3, TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2, TAU_3, TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2,
          TAU_3, TAU_3 * 2, TAU_3, TAU_3, TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2, TAU_3, TAU_3, TAU_3 * 2,
          TAU_3, TAU_3 * 2, TAU_3, TAU_3 * 2, TAU_3
        )
      )
  }

  it must "have a gonality value" in {
    p666_4by4_reticulate.gonality shouldBe
      1
  }

  it must "have perimeter length, area and compactness measured" in {
    (
      p666_4by4_reticulate.perimeterLength,
      p666_4by4_reticulate.area,
      p666_4by4_reticulate.compactness
    ) shouldBe
      (30, 41.569219381653056, 0.5804157965549497)
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (p666_4by4_reticulate.countSymmetries, p666_4by4_reticulate.countRotationalSymmetries) shouldBe
      (2, 2)
  }

  "An empty tiling" must "have an area" in {
    Tiling.empty.area shouldBe
      0
  }

  "A tiling choked by triangles"  must "have a gonality value" in {
    chokedByTriangles.gonality shouldBe
      4
  }

  val polygonPath: p4444_4by4_reticulate.PolygonPath =
    p4444_4by4_reticulate.PolygonPath(Vector(1, 2, 7, 6).map(Node(_)))

  "A polygon path of a tiling" can "be created" in {
    polygonPath.toPolygon shouldEqual
      Polygon(4)
  }

  "A perimeter circular path" can "be converted to ordered polygons" in {
    p4444_4by4_reticulate.perimeterOrderedPolygons.mapValues2(_.map(_.toSides)) shouldBe
      Map(
        5 -> Vector(4),
        10 -> Vector(4, 4),
        20 -> Vector(4, 4),
        1 -> Vector(4),
        6 -> Vector(4, 4),
        21 -> Vector(4),
        2 -> Vector(4, 4),
        22 -> Vector(4, 4),
        3 -> Vector(4, 4),
        16 -> Vector(4, 4),
        11 -> Vector(4, 4),
        23 -> Vector(4, 4),
        4 -> Vector(4, 4),
        15 -> Vector(4, 4),
        24 -> Vector(4, 4),
        25 -> Vector(4)
      ).mapKeys(Node(_))
  }

  it can "be converted to angles" in {
    p4444_4by4_reticulate.perimeterAngles shouldBe
      Map(
        5 -> TAU_4, 10 -> TAU_2, 20 -> TAU_2, 1 -> TAU_4,
        6 -> TAU_2, 21 -> TAU_4, 2 -> TAU_2, 22 -> TAU_2,
        3 -> TAU_2, 16 -> TAU_2, 11 -> TAU_2, 23 -> TAU_2,
        4 -> TAU_2, 15 -> TAU_2, 24 -> TAU_2, 25 -> TAU_4
      )
  }

  it can "be converted to 2D points" in {
    p4444_4by4_reticulate.perimeterPoints.almostEquals(
      Vector(
        Point(),
        Point(0, 1),
        Point(0, 2),
        Point(0, 3),
        Point(0, 4),
        Point(1, 4),
        Point(2, 4),
        Point(3, 4),
        Point(4, 4),
        Point(4, 3),
        Point(4, 2),
        Point(4, 1),
        Point(4, 0),
        Point(3, 0),
        Point(2, 0),
        Point(1, 0)
      )
    ) shouldBe
      true
  }

  it can "be mapped to coords" in {
    p4444_4by4_reticulate.perimeterCoords.almostEqualsMap(
      Map(
        Node(5)  -> Point(0, 4),
        Node(10) -> Point(1, 4),
        Node(20) -> Point(3, 4),
        Node(1)  -> Point(),
        Node(6)  -> Point(1, 0),
        Node(21) -> Point(4, 0),
        Node(2)  -> Point(0, 1),
        Node(22) -> Point(4, 1),
        Node(3)  -> Point(0, 2),
        Node(16) -> Point(3, 0),
        Node(11) -> Point(2, 0),
        Node(23) -> Point(4, 2),
        Node(4)  -> Point(0, 3),
        Node(15) -> Point(2, 4),
        Node(24) -> Point(4, 3),
        Node(25) -> Point(4, 4)
      )
    ) shouldBe
      true
  }

  "A path of a regular polygon" can "be created" in {
    p3464.PolygonPath(Vector(7, 18, 19, 20).map(Node(_))) shouldEqual
      Vector(7, 18, 19, 20)
  }

  it must "fail if smaller than size 3" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { p3464.PolygonPath(Vector(1, 2).map(Node(_))) }
    caught.getMessage shouldBe "Invalid number of sides: 2"
  }

  it must "fail if not a circular path" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { p3464.PolygonPath(Vector(1, 2, 4, 11).map(Node(_))) }
    caught.getMessage shouldBe "Invalid path: nodes 2 and 4 are not connected"
  }

  val notAPolygon: Vector[Node] =
    Vector(1, 2, 10, 9, 8).map(Node(_))

  it must "fail if not a regular polygon" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { p3464.PolygonPath(notAPolygon) }
    caught.getMessage shouldBe
      "Invalid regular polygon path: node 1 and node 9 are connected internally"
  }

  it must "fail if also not a regular polygon" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] {
        p3464.PolygonPath(Vector(9, 1, 2, 10, 27, 26, 25, 24).map(Node(_)))
      }
    caught.getMessage shouldBe
      "Invalid regular polygon path: node 9 and node 10 are connected internally"
  }

  it must "not fail if created unsafe" in {
    p3464.PolygonPath.unsafe(notAPolygon) shouldEqual
      Vector(1, 2, 10, 9, 8)
  }

  val t: Tiling =
    minimalDifferentFromItsPeri.toMaybeTiling.unsafe

  "A triHexOfSide3" can "be checked for reflectional and rotational symmetry" in {
    (p333333_grown_hexagon.countSymmetries, p333333_grown_hexagon.countRotationalSymmetries) shouldBe
      (6, 6)
  }

  "A hexHexOfSide3" can "be checked for reflectional and rotational symmetry" in {
    (p666_grown_hexagon.countSymmetries, p666_grown_hexagon.countRotationalSymmetries) shouldBe
      (6, 6)
  }

  "A edges12Nodes8" can "be checked for reflectional and rotational symmetry" in {
    (edges12Nodes8.countSymmetries, edges12Nodes8.countRotationalSymmetries) shouldBe
      (0, 1)
  }

  "A tiling" can "be checked for reflectional and rotational symmetry" in {
    (t.countSymmetries, t.countRotationalSymmetries) shouldBe
      (2, 2)
  }

  it can "be created from the polygons at a full vertex" in {
    Tiling.fromVertex(FullVertex.s("(3₆)").vertex).graphEdges.sorted(EdgeOrdering) shouldBe
      List(1--2, 1--3, 1--4, 1--5, 1--6, 1--7, 2--3, 2--7, 3--4, 4--5, 5--6, 6--7)
  }

  it can "be created from the polygons at a partial vertex" in {
    Tiling.fromVertex(Vertex(Vector.fill(3)(Polygon(3)))).graphEdges.sorted(EdgeOrdering) shouldBe
      List(1--2, 1--3, 1--4, 1--5, 2--3, 3--4, 4--5)
  }

  "Two square net of different sizes" must "have the same compactness" in {
    squareReticulate(3).compactness.~=(squareReticulate(4).compactness) shouldBe
      true
  }

  "The compactness of a tiling" must "increase as it nears a circle" in {
    val results: List[(Int, Double)] =
      List(
        (3, 0.6045997880780728),
        (4, 0.7853981633974484),
        (5, 0.86480626597721),
        (6, 0.9068996821171089),
        (7, 0.9319406234990958),
        (8, 0.9480594489685198),
        (9, 0.9590505418736094),
        (10, 0.9668827990464026)
      )
    (3 to 10)
      .toList
      .map(size => (size, Tiling.fromPolygon(size).compactness))
      .sortBy((_, compactness) => compactness)
      .zip(results)
      .forall({ case ((a, b), (c, d)) => a == c && b.~=(d) }) shouldBe
      true
  }
  
  "A tiling" can "have an empty dual graph" in {
    triangle.dual.graphEdges.isEmpty shouldBe
      true
  }
  
  it can "have a dual graph that is a valid tiling" in {
    p4444_4by4_reticulate.dual.toMaybeTiling.isRight shouldBe
      true
  }
  
  it can "have a dual graph" in {
    p333333_4by4_reticulate.dual.graphEdges shouldBe
      List(
        6--8, 9--10, 14--15, 3--4, 2--7, 11--12, 4--5, 2--8, 10--11,
        13--15, 5--6, 8--9, 10--14, 15--16, 1--4, 2--3, 7--11, 12--13
      )
  }

}
