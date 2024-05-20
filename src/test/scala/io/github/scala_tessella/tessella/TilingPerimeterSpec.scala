package io.github.scala_tessella.tessella

import Geometry.Radian
import Geometry.Radian.{TAU_2, TAU_3}
import Outliers.*
import RegularPolygon.Vertex
import TilingSymmetry.*
import Topology.{--, Node}

import io.github.scala_tessella.ring_seq.{IteratingOps, SymmetryOps}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingPerimeterSpec extends AnyFlatSpec with IteratingOps with SymmetryOps with Helper with should.Matchers {

  "The perimeter of a tiling" can "be described as a circular path of edges" in {
    edges12Nodes8.perimeter.toRingEdges.toList shouldBe
      List(1--2, 2--4, 4--6, 6--8, 7--8, 5--7, 3--5, 1--3)
  }

  "The perimeter of sqr4x4Reticulate" can "have symmetries" in {
    (
      sqr4x4Reticulate.perimeterRotationalSymmetry,
      sqr4x4Reticulate.orderedPerimeterAngles.symmetryIndices,
      sqr4x4Reticulate.perimeterReflectionSymmetry
    ) shouldBe
      (4, List(0, 4, 8, 12), 4)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    sqr4x4Reticulate.perimeterDistinctSymmetricNodes shouldBe
      Vector(11, 6, 1)
  }

  "The perimeter of tri4x4Reticulate" can "have symmetries" in {
    (
      tri4x4Reticulate.perimeterRotationalSymmetry,
      tri4x4Reticulate.orderedPerimeterAngles.symmetryIndices,
      tri4x4Reticulate.perimeterReflectionSymmetry
    ) shouldBe
      (2, Nil, 0)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    tri4x4Reticulate.perimeterDistinctSymmetricNodes shouldBe
      Vector(1, 2, 3, 6, 9, 12)
  }

  "The perimeter of hex4x4Reticulate" can "have symmetries" in {
    (
      hex4x4Reticulate.perimeterRotationalSymmetry,
      hex4x4Reticulate.orderedPerimeterAngles.symmetryIndices,
      hex4x4Reticulate.perimeterReflectionSymmetry
    ) shouldBe
      (2, List(0, 15), 2)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    hex4x4Reticulate.perimeterDistinctSymmetricNodes shouldBe
      Vector(8, 7, 6, 5, 4, 3, 2, 1)
  }

  val asymmetricalHex: Tiling =
    Tiling.hexagonNet(4, 3).unsafe

  "The perimeter of asymmetricalHex" can "have symmetries" in {
    (
      asymmetricalHex.perimeterRotationalSymmetry,
      asymmetricalHex.orderedPerimeterAngles.symmetryIndices,
      asymmetricalHex.perimeterReflectionSymmetry
    ) shouldBe
      (2, Nil, 0)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    asymmetricalHex.perimeterDistinctSymmetricNodes shouldBe
      Vector(1, 10, 11, 20, 21, 30, 31, 32, 33, 34, 35, 36, 37)
  }

  "The perimeter of edges12Nodes8" can "have symmetries" in {
    (
      edges12Nodes8.perimeterRotationalSymmetry,
      edges12Nodes8.orderedPerimeterAngles.symmetryIndices,
      edges12Nodes8.perimeterReflectionSymmetry
    ) shouldBe
      (1, Nil, 0)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    edges12Nodes8.perimeterDistinctSymmetricNodes shouldBe
      Vector(1, 2, 4, 6, 8, 7, 5, 3)
  }

  "The perimeter of closer" can "have symmetries" in {
    (
      closer.perimeterRotationalSymmetry,
      closer.orderedPerimeterAngles.symmetryIndices,
      closer.perimeterReflectionSymmetry
    ) shouldBe
      (1, Nil, 0)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    closer.perimeterDistinctSymmetricNodes shouldBe
      Vector(1, 3, 5, 7, 10, 9, 8, 6, 4, 2)
  }

  "The perimeter of pentagon" can "have symmetries" in {
    (
      pentagon.perimeterRotationalSymmetry,
      pentagon.orderedPerimeterAngles.symmetryIndices,
      pentagon.perimeterReflectionSymmetry
    ) shouldBe
      (5, List(0, 1, 2, 3, 4), 5)
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    pentagon.perimeterDistinctSymmetricNodes shouldBe
      Vector(1)
  }

  val p33336: Tiling =
    Tiling.fromFullVertex(FullVertex.s("(3₄.6)"))

  val angles: Vector[Radian] =
    p33336.orderedPerimeterAngles

  "The perimeter of p33336" can "have perimeter angles" in {
    angles shouldBe
      Vector(TAU_2, TAU_3, TAU_3, TAU_3, TAU_2, TAU_3, TAU_3, TAU_3)
  }

  it can "have angles with rotational symmetry 2" in {
    angles.rotationalSymmetry shouldBe
      2
  }

  it can "have angles with reflectional symmetry 2" in {
    angles.symmetry shouldBe
      2
  }

  it can "have reduced fillable nodes thanks to symmetries" in {
    p33336.perimeterDistinctSymmetricNodes shouldBe
      Vector(4, 3, 2, 9, 8)
  }

  val sizes: Vector[Vertex] =
    p33336.orderedPerimeterMinorVertices

  it can "have sizes of the vertices" in {
    sizes shouldBe
      Vector(
        Vector(3, 6),
        Vector(6),
        Vector(6),
        Vector(6),
        Vector(3, 6),
        Vector(3, 3),
        Vector(3, 3),
        Vector(3, 3)
      )
  }

  it can "have vertices sizes with rotational symmetry 1" in {
    sizes.rotationalSymmetry shouldBe
      1
  }

  it can "have vertices sizes with reflectional symmetry 1" in {
    sizes.symmetry shouldBe
      1
  }

  it can "have symmetries" in {
    (
      p33336.perimeterRotationalSymmetry,
      p33336.orderedPerimeterAngles.symmetryIndices,
      p33336.perimeterReflectionSymmetry
    ) shouldBe
      (1, List(0, 4), 1)
  }

//  "A tiling" can "return the nodes sorted by min exterior angle and min ordinal" in {
//    edges12Nodes8.perimeter.toNodesO.sorted(edges12Nodes8.MinAngleNodeOrdering.orElse(NodeOrdering)) shouldBe
//      Vector(4, 7, 2, 3, 5, 6, 1, 8)
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.NARROWEST_ANGLE_THEN_LOWEST_ORDINAL) shouldBe
//      Vector(4, 7, 2, 3, 5, 6, 1, 8).map(Node(_))
//  }
//
//  it can "return the nodes sorted by min ordinal" in {
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.LOWEST_ORDINAL) shouldBe
//      Vector(1, 2, 3, 4, 5, 6, 7, 8).map(Node(_))
//  }
//
//  it can "return the nodes sorted by max ordinal" in {
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.HIGHEST_ORDINAL) shouldBe
//      Vector(8, 7, 6, 5, 4, 3, 2, 1).map(Node(_))
//  }
//
//  it can "return the nodes sorted by max angle then max ordinal" in {
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.WIDEST_ANGLE_THEN_HIGHEST_ORDINAL) shouldBe
//      Vector(8, 1, 6, 5, 3, 2, 7, 4).map(Node(_))
//  }
//
//  it can "return the nodes sorted by max angle then min ordinal" in {
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.WIDEST_ANGLE_THEN_LOWEST_ORDINAL) shouldBe
//      Vector(1, 8, 6, 2, 3, 5, 7, 4).map(Node(_))
//  }
//
//  it can "return the nodes sorted by min angle then max ordinal" in {
//    edges12Nodes8.sortedPerimeterNodes(PerimeterStrategy.NARROWEST_ANGLE_THEN_LOWEST_ORDINAL) shouldBe
//      Vector(4, 7, 2, 3, 5, 6, 1, 8).map(Node(_))
//  }

//  it can "return the edges sorted by min combined exterior angle and min combined ordinal" in {
//    edges12Nodes8.perimeter.toRingEdges.toList.sorted(edges12Nodes8.MinAngleEdgeOrdering.orElse(EdgeOrdering)) shouldBe
//      List(2--4, 4--6, 5--7, 7--8, 3--5, 1--2, 1--3, 6--8)
//    edges12Nodes8.sortedPerimeterEdges(NARROWEST_ANGLE, LOWEST_ORDINAL) shouldBe
//      Vector(2--4, 4--6, 5--7, 7--8, 3--5, 1--2, 1--3, 6--8)
//  }
//
//  it can "return the edges sorted by min combined ordinal" in {
//    edges12Nodes8.sortedPerimeterEdges(LOWEST_ORDINAL) shouldBe
//      Vector(1--2, 1--3, 2--4, 3--5, 4--6, 5--7, 6--8, 7--8)
//  }
//
//  it can "return the edges sorted by max combined ordinal" in {
//    edges12Nodes8.sortedPerimeterEdges(HIGHEST_ORDINAL) shouldBe
//      Vector(7--8, 6--8, 5--7, 4--6, 3--5, 2--4, 1--3, 1--2)
//  }
//
//  it can "return the edges sorted by max combined exterior angle and max combined ordinal" in {
//    edges12Nodes8.sortedPerimeterEdges(WIDEST_ANGLE, HIGHEST_ORDINAL) shouldBe
//      Vector(6--8, 1--3, 1--2, 3--5, 7--8, 5--7, 4--6, 2--4)
//  }
//
//  it can "return the edges sorted by max combined exterior angle and min combined ordinal" in {
//    edges12Nodes8.sortedPerimeterEdges(WIDEST_ANGLE, LOWEST_ORDINAL) shouldBe
//      Vector(6--8, 1--2, 1--3, 3--5, 7--8, 5--7, 4--6, 2--4)
//  }
//
//  it can "return the edges sorted by min combined exterior angle and by max combined ordinal" in {
//    edges12Nodes8.sortedPerimeterEdges(NARROWEST_ANGLE, HIGHEST_ORDINAL) shouldBe
//      Vector(2--4, 4--6, 5--7, 7--8, 3--5, 1--3, 1--2, 6--8)
//  }

//  it can "return the perimeter nodes adjacent to a perimeter node sorted by perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(4), Node(8)), BEFORE_PERIMETER) shouldBe
//      (Node(4), Node(8))
//  }
//
//  it can "return them sorted by inverse perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(4), Node(8)), AFTER_PERIMETER) shouldBe
//      (Node(8), Node(4))
//  }
//
//  it can "return them sorted by higher ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(4), Node(8)), HIGHER_ORDINAL) shouldBe
//      (Node(8), Node(4))
//  }
//
//  it can "return them sorted by lower ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(4), Node(8)), LOWER_ORDINAL) shouldBe
//      (Node(4), Node(8))
//  }
//
//  it can "return them sorted by wider angle and then higher ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(4), Node(8)), WIDER_ANGLE, HIGHER_ORDINAL) shouldBe
//      (Node(8), Node(4))
//  }
//
//  it can "return another sorted by wider angle and then perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), WIDER_ANGLE, BEFORE_PERIMETER) shouldBe
//      (Node(6), Node(2))
//  }
//
//  it can "return another sorted by wider angle and then inverse perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), WIDER_ANGLE, AFTER_PERIMETER) shouldBe
//      (Node(6), Node(2))
//  }
//
//  it can "return another sorted by wider angle and then higher ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), WIDER_ANGLE, HIGHER_ORDINAL) shouldBe
//      (Node(6), Node(2))
//  }
//
//  it can "return another sorted by wider angle and then lower ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), WIDER_ANGLE, LOWER_ORDINAL) shouldBe
//      (Node(6), Node(2))
//  }
//
//  it can "return another sorted by narrower angle and then perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), NARROWER_ANGLE, BEFORE_PERIMETER) shouldBe
//      (Node(2), Node(6))
//  }
//
//  it can "return another sorted by narrower angle and then inverse perimeter direction" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), NARROWER_ANGLE, AFTER_PERIMETER) shouldBe
//      (Node(2), Node(6))
//  }
//
//  it can "return another sorted by narrower angle and then higher ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), NARROWER_ANGLE, HIGHER_ORDINAL) shouldBe
//      (Node(2), Node(6))
//  }
//
//  it can "return another sorted by narrower angle and then lower ordinal" in {
//    edges12Nodes8.sortedPerimeterNodesPair((Node(2), Node(6)), NARROWER_ANGLE, LOWER_ORDINAL) shouldBe
//      (Node(2), Node(6))
//  }

}