package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.{TAU_2, TAU_3, TAU_4, TAU_6}
import Outliers.*
import RegularPolygon.{Polygon, Vertex}
import TilingSymmetry.{countSymmetries, countRotationalSymmetries}
import Topology.*
import utility.Utils.{mapKeys, mapValues2}

import math.geom2d.Point2D
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TilingSpec extends AnyFlatSpec with should.Matchers {

  val path1: sqr4x4Reticulate.Path =
    sqr4x4Reticulate.Path(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25).map(Node(_)))

  val path2: sqr4x4Reticulate.Path =
    sqr4x4Reticulate.Path(Vector(1, 2, 7, 8).map(Node(_)))

  val ringPath1: sqr4x4Reticulate.PolygonPath =
    sqr4x4Reticulate.PolygonPath(Vector(1, 2, 7, 6).map(Node(_)))

  val ringPath2: sqr4x4Reticulate.PolygonPath =
    sqr4x4Reticulate.PolygonPath(Vector(3, 2, 7, 8).map(Node(_)))

  val ringPath3: sqr4x4Reticulate.PolygonPath =
    sqr4x4Reticulate.PolygonPath(Vector(6, 7, 12, 11).map(Node(_)))

  val ringPath4: sqr4x4Reticulate.PolygonPath =
    sqr4x4Reticulate.PolygonPath(Vector(8, 7, 12, 13).map(Node(_)))

  val full: List[sqr4x4Reticulate.PolygonPath] =
    List(ringPath1, ringPath2, ringPath3, ringPath4)

  "A tiling" must "have a perimeter as a circular path" in {
    sqr4x4Reticulate.perimeter shouldEqual
      Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 24, 23, 22, 21, 16, 11, 6)
  }

  it must "have a map of perimeter polygons as circular paths" in {
    sqr4x4Reticulate.perimeterOrderedPolygonPaths shouldEqual
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
    (pentagon.graphNodes.size, pentagon.edges.size) shouldBe
      (5, 5)
  }

  it can "NOT be publicly created with a constructor" in {
    "Tiling(triangle.edges)" shouldNot
      compile
  }

  it can " be publicly created with validation" in {
    "Tiling.maybe(triangle.edges)" should
      compile
  }

  it must "be publicly created as an Either" in {
    Tiling.maybe(triangle.edges).isRight shouldBe
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
    Tiling.maybe(triangle.edges ++ triangle.edges) shouldEqual
      Right(triangle)
  }

  it must "have a perimeter" in {
    triangle.perimeter.toRingEdges.toList.sorted(EdgeOrdering) shouldEqual
      triangle.edges.sorted(EdgeOrdering)
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
    val expected: Vector[Point2D] =
      Vector(Point2D(0, 0), Point2D(0.5, S6), Point2D(1, 0))
    triangle.perimeterPoints2D.almostEquals(expected) shouldBe
      true
  }

  it can "have a map of nodes and cartesian points" in {
    triangle.perimeterCoords.almostEqualsMap(Map(
      1 -> Point2D(0, 0),
      3 -> Point2D(0.5, S6),
      2 -> Point2D(1, 0)
    ).mapKeys(Node(_))) shouldBe
      true
  }

  "A sqr4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      sqr4x4Reticulate.perimeter.toRingNodes,
      sqr4x4Reticulate.orderedPerimeterAngles,
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
      sqr4x4Reticulate.perimeterLength,
      sqr4x4Reticulate.area,
      sqr4x4Reticulate.compactness
    ) shouldBe
      (16, 16.000000000000004, 0.7853981633974484)
  }

  it must "have an area calculated with an alternative method" in {
    (sqr4x4Reticulate.areaAlt ~= 16) shouldBe
      true
  }

  it can "transform perimeter into cartesian points" in {
    val expected: Vector[Point2D] =
      Vector(
        Point2D(0, 0), Point2D(0, 1), Point2D(0, 2), Point2D(0, 3), Point2D(0, 4), Point2D(1, 4), Point2D(2, 4),
        Point2D(3, 4), Point2D(4, 4), Point2D(4, 3), Point2D(4, 2), Point2D(4, 1), Point2D(4, 0), Point2D(3, 0),
        Point2D(2, 0), Point2D(1, 0)
      )
    sqr4x4Reticulate.perimeterPoints2D.almostEquals(expected) shouldBe
      true
  }

  it must "have a gonality value" in {
    sqr4x4Reticulate.gonality shouldBe
      1
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (sqr4x4Reticulate.countSymmetries, sqr4x4Reticulate.countRotationalSymmetries) shouldBe
      (4, 4)
  }

  "A tri4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      tri4x4Reticulate.perimeter.toRingNodes,
      tri4x4Reticulate.orderedPerimeterAngles,
    ) shouldBe
      (
        Vector(1, 2, 3, 6, 9, 12, 15, 14, 13, 10, 7, 4),
        Vector(TAU_3, TAU_2, TAU_6, TAU_2, TAU_2, TAU_2, TAU_3, TAU_2, TAU_6, TAU_2, TAU_2, TAU_2)
      )
  }

  it must "have perimeter length, area and compactness measured" in {
    (
      tri4x4Reticulate.perimeterLength,
      tri4x4Reticulate.area,
      tri4x4Reticulate.compactness
    ) shouldBe
      (12, 6.928203230275511, 0.6045997880780728)
  }

  it must "have a gonality value" in {
    tri4x4Reticulate.gonality shouldBe
      1
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (tri4x4Reticulate.countSymmetries, tri4x4Reticulate.countRotationalSymmetries) shouldBe
      (0, 2)
  }

  "A hex4x4Reticulate" can "have perimeter nodes and perimeter path angles" in {
    (
      hex4x4Reticulate.perimeter.toRingNodes,
      hex4x4Reticulate.orderedPerimeterAngles,
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
    hex4x4Reticulate.gonality shouldBe
      1
  }

  it must "have perimeter length, area and compactness measured" in {
    (
      hex4x4Reticulate.perimeterLength,
      hex4x4Reticulate.area,
      hex4x4Reticulate.compactness
    ) shouldBe
      (30, 41.569219381653056, 0.5804157965549497)
  }

  it can "be checked for reflectional and rotational symmetry" in {
    (hex4x4Reticulate.countSymmetries, hex4x4Reticulate.countRotationalSymmetries) shouldBe
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

  val polygonPath: sqr4x4Reticulate.PolygonPath =
    sqr4x4Reticulate.PolygonPath(Vector(1, 2, 7, 6).map(Node(_)))

  "A polygon path of a tiling" can "be created" in {
    polygonPath.toPolygon shouldEqual
      Polygon(4)
  }

  "A perimeter circular path" can "be converted to ordered polygons" in {
    sqr4x4Reticulate.perimeterOrderedPolygons.mapValues2(_.map(_.toSides)) shouldBe
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
    sqr4x4Reticulate.perimeterAngles shouldBe
      Map(
        5 -> TAU_4, 10 -> TAU_2, 20 -> TAU_2, 1 -> TAU_4,
        6 -> TAU_2, 21 -> TAU_4, 2 -> TAU_2, 22 -> TAU_2,
        3 -> TAU_2, 16 -> TAU_2, 11 -> TAU_2, 23 -> TAU_2,
        4 -> TAU_2, 15 -> TAU_2, 24 -> TAU_2, 25 -> TAU_4
      )
  }

  it can "be converted to 2D points" in {
    sqr4x4Reticulate.perimeterPoints2D.almostEquals(
      Vector(
        Point2D(0, 0),
        Point2D(0, 1),
        Point2D(0, 2),
        Point2D(0, 3),
        Point2D(0, 4),
        Point2D(1, 4),
        Point2D(2, 4),
        Point2D(3, 4),
        Point2D(4, 4),
        Point2D(4, 3),
        Point2D(4, 2),
        Point2D(4, 1),
        Point2D(4, 0),
        Point2D(3, 0),
        Point2D(2, 0),
        Point2D(1, 0)
      )
    ) shouldBe
      true
  }

  it can "be mapped to coords" in {
    sqr4x4Reticulate.perimeterCoords.almostEqualsMap(
      Map(
        Node(5)  -> Point2D(0, 4),
        Node(10) -> Point2D(1, 4),
        Node(20) -> Point2D(3, 4),
        Node(1)  -> Point2D(0, 0),
        Node(6)  -> Point2D(1, 0),
        Node(21) -> Point2D(4, 0),
        Node(2)  -> Point2D(0, 1),
        Node(22) -> Point2D(4, 1),
        Node(3)  -> Point2D(0, 2),
        Node(16) -> Point2D(3, 0),
        Node(11) -> Point2D(2, 0),
        Node(23) -> Point2D(4, 2),
        Node(4)  -> Point2D(0, 3),
        Node(15) -> Point2D(2, 4),
        Node(24) -> Point2D(4, 3),
        Node(25) -> Point2D(4, 4)
      )
    ) shouldBe
      true
  }

  "A path of a regular polygon" can "be created" in {
    triSqrHexHexoid.PolygonPath(Vector(7, 18, 19, 20).map(Node(_))) shouldEqual
      Vector(7, 18, 19, 20)
  }

  it must "fail if smaller than size 3" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { triSqrHexHexoid.PolygonPath(Vector(1, 2).map(Node(_))) }
    caught.getMessage shouldBe "Invalid number of sides: 2"
  }

  it must "fail if not a circular path" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { triSqrHexHexoid.PolygonPath(Vector(1, 2, 4, 11).map(Node(_))) }
    caught.getMessage shouldBe "Invalid path: nodes 2 and 4 are not connected"
  }

  val notAPolygon: Vector[Node] =
    Vector(1, 2, 10, 9, 8).map(Node(_))

  it must "fail if not a regular polygon" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { triSqrHexHexoid.PolygonPath(notAPolygon) }
    caught.getMessage shouldBe
      "Invalid regular polygon path: node 9 and node 1 are connected internally"
  }

  it must "fail if also not a regular polygon" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] {
        triSqrHexHexoid.PolygonPath(Vector(9, 1, 2, 10, 27, 26, 25, 24).map(Node(_)))
      }
    caught.getMessage shouldBe
      "Invalid regular polygon path: node 9 and node 2 are connected internally"
  }

  it must "not fail if created unsafe" in {
    triSqrHexHexoid.PolygonPath.unsafe(notAPolygon) shouldEqual
      Vector(1, 2, 10, 9, 8)
  }

  val t: Tiling =
    minimalDifferentFromItsPeri.toMaybeTiling.unsafe

  "A triHexOfSide3" can "be checked for reflectional and rotational symmetry" in {
    (triHexOfSide3.countSymmetries, triHexOfSide3.countRotationalSymmetries) shouldBe
      (6, 6)
  }

  "A hexHexOfSide3" can "be checked for reflectional and rotational symmetry" in {
    (hexHexOfSide3.countSymmetries, hexHexOfSide3.countRotationalSymmetries) shouldBe
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
    Tiling.fromVertex(FullVertex.s("(3⁶)").vertex).edges.sorted(EdgeOrdering) shouldBe
      List(1--2, 1--3, 1--4, 1--5, 1--6, 1--7, 2--3, 2--7, 3--4, 4--5, 5--6, 6--7)
  }

  it can "be created from the polygons at a partial vertex" in {
    Tiling.fromVertex(Vertex(Vector.fill(3)(Polygon(3)))).edges.sorted(EdgeOrdering) shouldBe
      List(1--2, 1--3, 1--4, 1--5, 2--3, 3--4, 4--5)
  }

  "Two square net of different sizes" must "have the same compactness" in {
    squareReticulate(3).compactness.~=(squareReticulate(4).compactness) shouldBe
      true
  }

  "The compactness of a tiling" must "increase as it nears a circle" in {
    (3 to 10)
      .toList
      .map(size => (size, Tiling.fromPolygon(size).compactness))
      .sortBy((_, compactness) => compactness) shouldBe
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
  }
}
