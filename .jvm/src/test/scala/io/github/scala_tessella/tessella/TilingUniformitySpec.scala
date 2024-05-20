package io.github.scala_tessella.tessella

import Outliers.*
import RegularPolygon.Vertex
import Topology.{--, Edge, Node}
import TilingUniformity.*
import utility.Utils.{mapKeys, mapValues2}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingUniformitySpec extends AnyFlatSpec with Helper with should.Matchers {

  "A test tiling" can "return the nodes belonging to each different gonality" in {
    gonExperiment.groupGonals shouldBe
      Map(
        Vector(4, 4, 4, 4) -> List(1, 2, 3, 4),
        Vector(4, 4, 8) -> List(14, 15),
        Vector(4, 4, 6) -> List(9, 8),
        Vector(3, 3, 4, 4) -> List(5)
      ).mapKeys(_.map(Node(_)))
  }

  val origins: List[Node] =
    gonExperiment.groupGonals.values.flatten.toList

  it must "have origins" in {
    origins shouldBe
      List(9, 8, 1, 2, 3, 4, 14, 15, 5)
  }

  val matrixGon: Map[Node, List[(List[Int], Boolean)]] =
    gonExperiment.outerOrderedStripFrom(origins)

  it must "have a matrix" in {
    matrixGon shouldBe
      Map(
        5 -> List((List(3, 3, 4, 4), false), (List(4), false), (List(), false), (List(), false), (List(), false)),
        14 -> List((List(4, 8, 4), false), (List(4), false), (List(), false), (List(), false)),
        1 -> List((List(4, 4, 4, 4), true), (List(), false), (List(), false), (List(), false)),
        9 -> List((List(4, 4, 6), false), (List(4), false), (List(), false), (List(), false), (List(), false), (List(), false)),
        2 -> List((List(4, 4, 4, 4), true), (List(), false), (List(), false), (List(), false), (List(), false)),
        3 -> List((List(4, 4, 4, 4), true), (List(), false), (List(), false), (List(), false)),
        8 -> List((List(4, 4, 6), false), (List(4), false), (List(), false), (List(), false), (List(), false)),
        4 -> List((List(4, 4, 4, 4), true), (List(), false), (List(), false), (List(), false)),
        15 -> List((List(4, 4, 8), false), (List(4), false), (List(), false), (List(), false), (List(), false))
      ).map((key, value) => Node(key) -> value)
  }

//  it must "have a tree for uniform nodes" in {
//    gonExperiment.uniformNodesTree(matrix) shouldBe
//      Map(
//        List(0) -> List(1, 2, 3, 4),
//        List(1) -> List(15, 8, 9, 14, 5)
//      )
//  }

  it must "have a gonality map" in {
    gonExperiment.groupGonals shouldBe
      Map(
        Vertex(4, 4, 6) -> List(9, 8),
        Vertex(4, 4, 4, 4) -> List(1, 2, 3, 4),
        Vertex(4, 4, 8) -> List(14, 15),
        Vertex(3, 3, 4, 4) -> List(5)
      )
  }

  it must "have a way to group uniforms" in {
    gonExperiment.groupUniforms shouldBe
      Map(
        (Vertex(4, 4, 8), List()) -> Vector(14, 15),
        (Vertex(4, 4, 4, 4), List()) -> Vector(1, 2, 3, 4),
        (Vertex(4, 4, 6), List()) -> Vector(9, 8),
        (Vertex(3, 3, 4, 4), List()) -> Vector(5)
      )
  }

  it can "extend the uniformity to other nodes" in {
    gonExperiment.groupUniformsNestedComplete shouldBe
      Map(
        Vector(4, 4, 6) -> List((List(), Vector(10, 9, 7, 8))),
        Vector(4, 4, 4, 4) -> List((List(), Vector(1, 6, 2, 12, 3, 11, 4))),
        Vector(4, 4, 8) -> List((List(), Vector(14, 13, 15))),
        Vector(3, 3, 4, 4) -> List((List(), Vector(5)))
      ).mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "have an uniformity value 4" in {
    gonExperiment.uniformity shouldBe
      4
  }

  val origins2: List[Node] =
    uniformityIssue4.groupGonals.values.flatten.toList

  "An uniformityIssue4" must "have origins" in {
    origins2 shouldBe
      List(
        69, 115, 217, 5, 120, 10, 142, 185, 42, 14, 184, 20, 46, 93, 78, 164, 211, 121, 74, 206, 116, 6, 117, 70, 165,
        38, 21, 92, 229, 97, 9, 188, 53, 141, 193, 212, 96, 124, 73, 205, 166, 148, 45, 180, 17, 34, 22, 44, 27, 59,
        118, 71, 12, 54, 144, 49, 181, 187, 172, 230, 76, 39, 98, 208, 103, 140, 91, 66, 167, 35, 209, 145, 48, 63, 18,
        95, 50, 67, 16, 127, 31, 182, 11, 72, 143, 43, 231, 40, 26, 186, 139, 23, 55, 8, 207, 214, 119, 58, 151, 168,
        146, 30, 190, 183, 19, 210, 79, 94, 4, 15, 68, 62, 47, 163, 122, 232, 100, 90, 101, 88, 56, 153, 24, 37, 25, 52,
        125, 189, 152, 57, 29, 179, 147, 61, 1, 89, 60, 33, 28, 65, 169, 77, 13, 129, 41, 2, 128, 105, 64, 149, 32, 191,
        219, 81, 7, 213, 3, 80, 123, 194, 99, 218, 104, 171, 75, 235, 36, 51, 195, 178, 215
      )
  }

  val matrix2filtered: Map[Node, List[(List[Int], Boolean)]] =
    uniformityIssue4.outerOrderedStripFrom(List(Node(7), Node(218), Node(219)))

  it must "have a matrix" in {
    matrix2filtered shouldBe
      Map(
        7 -> List(
          (List(6, 4, 3, 4), true),
          (List(4, 4, 12, 12, 6, 4, 6, 12, 12), true),
          (List(4, 6, 4, 6, 4, 6, 4, 6, 6, 3, 3, 6, 6, 4, 6, 4, 6, 4, 6, 4, 4, 4, 12, 12, 12, 4, 4), true),
          (List(
            3, 4, 12, 4, 3, 4, 12, 4, 3, 4, 12, 4, 4, 4, 12, 4, 3, 4, 12, 4, 3, 4,
            12, 4, 3, 4, 12, 4, 3, 3, 6, 6, 4, 6, 4, 6, 4, 6, 6, 3, 3, 4, 12, 4), true),
          (List(
            3, 4, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4,
            6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6), false),
          (List(3, 3), false)
        ),
        218 -> List(
          (List(6, 4, 3, 4), true),
          (List(12, 12, 4, 4), false),
          (List(4, 6, 4, 6, 6, 3, 3, 6, 6, 4, 6, 4), false),
          (List(3, 4, 12, 4, 3, 4, 12, 4, 4, 4, 12, 4, 3, 4, 12, 4, 3), false),
          (List(4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4), false),
          (List(3, 4, 12, 4, 3, 3, 4, 12, 4, 3, 4, 12, 4, 3, 3, 4, 12, 4, 3, 4, 12, 4, 3), false),
          (List(4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4), false),
          (List(3, 4, 12, 4, 3, 3, 4, 12, 4, 3, 4, 12, 4, 3, 3, 4, 12, 4, 3), false),
          (List(4, 6, 6, 6, 6, 4), false), (List(3, 3), false)
        ),
        219 -> List(
          (List(4, 6, 4, 3), true),
          (List(12, 12, 6), false),
          (List(4, 6, 4, 6, 4, 4, 4), false),
          (List(4, 6, 4, 6, 6, 3, 3, 4, 12, 4, 3, 4, 12, 4, 3), false),
          (List(3, 4, 12, 4, 3, 4, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4, 6, 4), false),
          (List(4, 6, 4, 6, 4, 4, 3, 4, 12, 4, 3, 4, 12, 4, 3, 3, 4, 12, 4, 3), false),
          (List(4, 6, 4, 6, 6, 6, 6, 4, 6, 4, 6, 4, 6, 4, 4, 3, 4, 12, 4, 3), false),
          (List(3, 4, 12, 4, 3, 3, 4, 12, 4, 3, 4, 12, 4, 3, 4, 4, 6, 4), false),
          (List(3, 4, 4, 6, 4, 6, 4, 6, 4, 6, 6, 6, 6, 4), false),
          (List(3, 3), false)
        )
      )
        .map((key, value) => Node(key) -> value)
  }

//  it must "have a filtered tree for uniform nodes" in {
//    uniformityIssue4.uniformNodesTree(matrix2filtered) shouldBe
//      Map(
//        List() -> List(7, 218, 219)
//      )
//  }

  val origins3: List[Node] =
    uniformityIssue6.groupGonals.values.flatten.toList

  "An uniformityIssue6" must "have origins" in {
    origins3 shouldBe
      List(
        1, 10, 21, 9, 13, 22, 44, 7, 39, 26, 19, 5, 6, 2, 8, 4, 42, 37, 25, 14, 20, 33, 38, 17, 27, 12, 3, 18, 16, 11
      )
  }

  val matrix3filtered: Map[Node, List[(List[Int], Boolean)]] =
    uniformityIssue6.outerOrderedStripFrom(List(Node(42), Node(2)))

  it must "have a matrix" in {
    matrix3filtered shouldBe
      Map(
        42 -> List(
          (List(3, 4, 3, 3, 4), true),
          (List(3), false),
          (List(4, 4, 3, 3), false),
          (List(3, 4, 3, 4), false),
          (List(3, 3, 3, 3, 4), false),
          (List(4, 4, 4, 3, 3, 3, 3), false),
          (List(3, 3, 3, 4, 4, 3), false),
          (List(3, 3, 4, 3, 3), false),
          (List(3), false),
          (List(), false)
        ),
        2 -> List(
          (List(4, 3, 4, 3, 3), true),
          (List(4, 3, 3, 3, 4, 3, 3), true),
          (List(4, 3, 4, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 4), true),
          (List(3, 3, 4, 4, 3, 3, 4, 3, 4, 3, 3, 4, 3, 4, 3, 3, 3), true),
          (List(), false),
          (List(), false),
          (List(), false)
        )
      )
        .map((key, value) => Node(key) -> value)
  }

//  it must "have a filtered tree for uniform nodes" in {
//    uniformityIssue6.uniformNodesTree(matrix3filtered) shouldBe
//      Map(
//        List() -> List(42, 2)
//      )
//  }

  "A sqr4x4Reticulate" can "return the nodes belonging to each different gonality" in {
    sqr4x4Reticulate.groupGonals shouldBe
      Map(
        Vector(4, 4, 4, 4) -> List(14, 9, 13, 17, 12, 7, 18, 8, 19)
      ).mapKeys(_.map(Node(_)))
  }

  it can "have its nodes grouped in uniform sets" in {
    sqr4x4Reticulate.groupUniforms shouldBe
      Map(
        (Vector(4, 4, 4, 4), List()) -> Vector(14, 9, 13, 17, 12, 7, 18, 8, 19)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have an uniformity value 1" in {
    sqr4x4Reticulate.uniformity shouldBe
      1
  }

  "Another tiling" can "return the nodes belonging to each different gonality" in {
    closer.groupGonals shouldBe
      Map(
        Vector(3, 3, 4) -> List(4),
        Vector(3, 3, 3, 3, 3) -> List(7)
      ).mapKeys(_.map(Node(_)))
  }

  "A tricky 3-uniform pattern [(3.4₂.6);2x(3.4.6.4)]" can "have its nodes grouped in uniform sets" in {
    earlyUniform3.groupUniforms shouldBe
      Map(
        (Vector(3, 4, 6, 4), List(0)) -> Vector(79, 56, 57, 3, 16, 55, 58, 4, 15, 64, 17, 63, 18),
        (Vector(3, 4, 6, 4), List(1)) -> Vector(10, 6, 21, 53, 45, 7),
        (Vector(3, 4, 4, 6), List()) ->
          Vector(
            69, 5, 38, 65, 9, 32, 31, 11, 19, 94, 24, 37, 52, 14, 20, 46,
            93, 1, 74, 73, 2, 22, 44, 59, 54, 86, 40, 23, 8, 82, 47, 62
          )
      ).mapValues2(_.map(Node(_)))
  }

  it can "have them grouped by vertex" in {
    earlyUniform3.groupUniformsNestedComplete shouldBe
      Map(
        Vector(3, 4, 6, 4) -> List(
          (List(0), Vector(56, 57, 89, 85, 64, 17, 3, 80, 63, 18, 16, 55, 58, 4, 79, 15, 90, 83)),
          (List(1),
            Vector(
              10, 25, 61, 6, 60, 102, 21, 33, 92, 53, 96, 13, 41, 45,
              12, 81, 76, 7, 39, 66, 48, 72, 99, 87, 75, 36, 30, 51
            ))
        ),
        Vector(3, 4, 4, 6) -> List(
          (List(),
            Vector(
              5, 84, 69, 101, 88, 42, 24, 37, 52, 14, 20, 46, 93, 78, 29, 1, 74, 70, 28, 38, 65, 97, 9, 77, 73, 2, 32,
              34, 22, 44, 59, 27, 71, 54, 49, 86, 98, 103, 91, 35, 95, 50, 67, 31, 11, 43, 40, 26, 23, 8, 82, 19, 94,
              47, 68, 62, 100
            ))
        )
      )
        .mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "have uniformity value 3" in {
    earlyUniform3.uniformity shouldBe
      3
  }

  "A triangleTriangleOfSide5" can "have its nodes grouped in uniform sets" in {
    triangleTriangleOfSide5.groupUniforms shouldBe
      Map(
        (Vector(3, 3, 3, 3, 3, 3), List()) -> Vector(5, 14, 9, 13, 12, 8)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 1" in {
    triangleTriangleOfSide5.uniformity shouldBe
      1
  }

  "A hexTrianguloidOfSide4" can "have its nodes grouped in uniform sets" in {
    hexTrianguloidOfSide4.groupUniforms shouldBe
      Map(
        (Vector(6, 6, 6), List()) -> Vector(14, 13, 22, 12, 16, 23, 15, 28, 21)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 1" in {
    hexTrianguloidOfSide4.uniformity shouldBe
      1
  }

  "A uniform3gonal2" can "have its nodes grouped in uniform sets" in {
    uniform3gonal2.groupUniforms shouldBe
      Map(
        (Vector(3, 3, 3, 3, 3, 3), List(0)) -> Vector(51, 55, 50, 53, 56, 57, 54),
        (Vector(3, 3, 3, 3, 6), List()) -> Vector(14, 33, 13, 35, 15, 24, 37, 34, 17, 22, 27, 26, 23),
        (Vector(3, 3, 3, 3, 3, 3), List(1)) -> Vector(25, 32, 12, 16, 36)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 3" in {
    uniform3gonal2.uniformity shouldBe
      3
  }

  "A 5-uniform 3-Archimedean tiling" can "have its nodes grouped in uniform sets" in {
    uniform5gonal3.groupUniforms shouldBe
      Map(
        (Vector(3, 3, 3, 3, 3, 3), List(0)) -> Vector(101, 106, 102, 113, 108, 104),
        (Vector(3, 3, 3, 3, 6), List(1)) -> Vector(37, 78, 33, 48, 23, 47, 34, 64),
        (Vector(3, 3, 3, 3, 6), List(0)) -> Vector(52, 60, 21, 32, 59, 49, 80, 51, 50, 31),
        (Vector(3, 3, 3, 3, 3, 3), List(1)) -> Vector(98, 116, 109, 105, 112),
        (Vector(3, 3, 6, 6), List()) -> Vector(25, 46, 65, 45, 76, 66, 35, 16, 75, 36)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 5 and gonality value 3" in {
    (uniform5gonal3.gonality, uniform5gonal3.uniformity) shouldBe
      (3, 5)
  }

  "Another tricky pattern" can "have its nodes grouped in uniform sets" in {
    uniformTricky.groupUniforms shouldBe
      Map(
        (Vector(3, 3, 3, 3, 3, 3), List()) -> Vector(1),
        (Vector(3, 3, 4, 3, 4), List(1)) -> Vector(9, 10),
        (Vector(3, 3, 4, 3, 4), List(0)) -> Vector(8, 11, 3, 7, 2)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 3" in {
    uniformTricky.uniformity shouldBe
      3
  }

  "A 7 uniform tiling [(3₆);2x(3₂.6₂);4x(6₃)]" can "have its nodes grouped in uniform sets" in {
    uniform7gonal3.groupUniforms shouldBe
      Map(
        (Vector(6, 6, 6), List(1)) -> Vector(111, 159, 33, 75, 123, 69, 138, 84, 61, 137, 97, 112, 48, 83, 76, 98, 47, 62),
        (Vector(3, 3, 6, 6), List(1)) -> Vector(110, 92, 153, 78, 64, 49, 81, 67, 154, 139, 82, 77, 96, 63, 95),
        (Vector(6, 6, 6), List(2, 1)) -> Vector(132, 131, 103, 41, 42, 117, 118),
        (Vector(3, 3, 3, 3, 3, 3), List()) -> Vector(185, 184, 179, 180, 181, 182, 183),
        (Vector(3, 3, 6, 6), List(0)) -> Vector(155, 156, 93, 65, 66, 80, 79, 94),
        (Vector(6, 6, 6), List(2, 0)) -> Vector(133, 58, 43, 130, 44, 134, 102, 120, 101, 115, 116, 119),
        (Vector(6, 6, 6), List(0)) -> Vector(46, 121, 45, 59, 113, 135, 99, 114, 136, 100, 60, 85)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have them grouped by vertex" in {
    uniform7gonal3.groupUniformsNestedComplete shouldBe
      Map(
        Vector(3, 3, 6, 6) -> List(
          (List(0), Vector(170, 142, 52, 93, 65, 156, 169, 141, 66, 155, 108, 3, 80, 18, 51, 4, 79, 94)),
          (List(1), Vector(5, 153, 110, 125, 157, 20, 78, 1, 6, 92, 53, 109, 77, 96, 2, 64, 49, 172, 81, 140, 91, 167, 35, 63, 95, 50, 67, 154, 143, 158, 171, 139, 82, 168, 19, 68))
        ),
        Vector(6, 6, 6) -> List(
          (List(1), Vector(69, 138, 174, 152, 84, 61, 160, 70, 165, 33, 21, 137, 97, 124, 173, 166, 34, 22, 159, 76, 7, 98, 112, 123, 48, 8, 75, 151, 36, 126, 62, 47, 83, 90, 111)),
          (List(2, 1), Vector(56, 42, 14, 132, 89, 117, 28, 13, 41, 27, 118, 103, 145, 104, 55, 146, 131)),
          (List(2, 0), Vector(101, 88, 115, 120, 25, 57, 29, 106, 147, 116, 133, 102, 129, 134, 105, 148, 44, 12, 54, 144, 39, 130, 162, 16, 177, 11, 72, 43, 87, 40, 26, 119, 58, 30, 15, 178)),
          (List(0), Vector(10, 24, 37, 46, 164, 121, 74, 85, 60, 38, 9, 73, 128, 45, 161, 17, 149, 32, 176, 59, 71, 86, 113, 135, 150, 127, 31, 175, 99, 23, 114, 107, 136, 163, 122, 100))
        ),
        Vector(3, 3, 3, 3, 3, 3) -> List(
          (List(), Vector(185, 184, 179, 180, 181, 182, 183))
        )
      ).mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "have uniformity value 7 and gonality value 3" in {
    (uniform7gonal3.gonality, uniform7gonal3.uniformity) shouldBe
      (3, 7)
  }

  "A uniform5v1 [2x(3₃.4₂);2x(3.4₂.6);(3.6.3.6)]" can "have its nodes grouped in uniform sets" in {
    uniform5v1.groupUniforms shouldBe
      Map(
        (Vector(3, 4, 4, 6), List(1)) -> Vector(90, 242, 190, 146, 246, 104, 240, 236, 176, 148, 244, 160, 102, 88, 234, 158, 188, 238, 174, 184, 152, 92, 156, 96, 180, 98, 150, 182, 154, 186, 94, 178, 100),
        (Vector(3, 3, 3, 4, 4), List(0)) -> Vector(222, 122, 30, 36, 26, 194, 226, 108, 208, 22, 34, 32, 28, 228, 24, 142, 214, 218, 224, 220, 216, 138, 130, 112, 114, 136, 200, 120, 202, 110, 196, 132, 116, 206, 134, 128, 118, 204, 140, 198),
        (Vector(3, 4, 4, 6), List(0)) -> Vector(239, 235, 175, 177, 91, 103, 245, 187, 159, 161, 237, 233, 243, 89, 147, 101, 241, 189, 247, 153, 185, 157, 93, 179, 97, 149, 181, 155, 95, 99, 151, 183),
        (Vector(3, 3, 3, 4, 4), List(1)) -> Vector(215, 227, 23, 31, 35, 223, 219, 27, 225, 141, 33, 29, 25, 221, 217, 115, 121, 133, 137, 197, 109, 129, 205, 113, 135, 199, 203, 139, 195, 131, 111, 117, 201, 119, 207),
        (Vector(3, 6, 3, 6), List()) -> Vector(170, 257, 78, 85, 165, 252, 169, 256, 166, 81, 251, 255, 250, 171, 82, 168, 79, 164, 253, 84, 80, 167, 254, 83)
      ).mapValues2(_.map(Node(_)))
  }

  it can "have uniformity value 5" in {
    uniform5v1.uniformity shouldBe
      5
  }

  "A 6-uniform pattern" can "have its nodes grouped by vertex in uniform sets" in {
    uniform6.groupUniformsNestedComplete shouldBe
      Map(
        Vector(3, 3, 3, 3, 6) -> List(
          (List(1), Vector(3)),
          (List(0), Vector(5, 52, 1, 6, 2, 4, 10, 56, 42, 24, 25, 20, 46, 57, 29, 21, 33, 28, 38, 13, 41, 17, 32, 34, 44, 27, 12, 54, 49, 7, 39, 35, 48, 50, 16, 31, 11, 43, 40, 26, 55, 23, 8, 58, 36, 30, 51, 19, 47, 15)
          )
        ),
        Vector(3, 6, 3, 6) -> List(
          (List(), Vector(9, 18))
        )
      ).mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "NOT be detected, at this early stage" in {
    (uniform6.gonality, uniform6.uniformity) shouldBe
      (2, 3)
  }

  "Another tricky x-uniform pattern" can "have its nodes grouped by vertex in uniform sets" in {
    uniform10.groupUniformsNestedComplete shouldBe
      Map(
        Vector(3, 4, 6, 4) -> List(
          (List(0, 1, 0), Vector(13, 69, 29, 33, 26)),
          (List(0, 0), Vector(5, 14, 74, 28, 32, 34, 59, 12, 30)),
          (List(0, 1, 1), Vector(37, 2, 17, 8, 36)),
          (List(1), Vector(83, 52, 6, 73, 64, 54, 81, 11, 58, 15)),
          (List(0, 2), Vector(84, 10, 57))
        ),
        Vector(3, 3, 4, 3, 4) -> List(
          (List(1), Vector(65, 87, 88, 56, 20, 46, 1, 85, 53, 45, 44, 71, 76, 7, 3, 35, 16, 43, 23, 82, 79, 68)),
          (List(0), Vector(77, 18, 24, 25, 78, 61, 70, 27, 31, 19, 62)),
          (List(2), Vector(66, 89, 60, 21, 38, 9, 22, 86, 80, 67, 75, 4))
        )
      ).mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "have uniformity value 8" in {
    uniform10.uniformity shouldBe
      8
  }

  val edges: List[Edge] =
    List(
      9 -- 2, 2 -- 3, 3 -- 4, 4 -- 5, 5 -- 6, 6 -- 7, 7 -- 8, 8 -- 9, 1 -- 6, 1 -- 5, 1 -- 4, 1 -- 3, 1 -- 2, 2 -- 10, 10 -- 9, 10 -- 11, 11 -- 12,
      12 -- 13, 13 -- 14, 14 -- 9, 14 -- 8, 4 -- 15, 15 -- 16, 16 -- 17, 17 -- 18, 18 -- 3, 18 -- 19, 19 -- 3, 19 -- 2, 10 -- 19, 5 -- 20, 20 -- 4,
      20 -- 15, 19 -- 21, 21 -- 10, 21 -- 11, 18 -- 22, 22 -- 23, 23 -- 24, 24 -- 21, 17 -- 22, 24 -- 25, 25 -- 21, 25 -- 11, 25 -- 26, 26 -- 11,
      26 -- 12, 6 -- 27, 27 -- 28, 28 -- 29, 29 -- 30, 30 -- 5, 30 -- 20, 7 -- 27, 30 -- 31, 31 -- 20, 31 -- 32, 32 -- 33, 33 -- 34, 34 -- 15,
      34 -- 16, 29 -- 35, 35 -- 30, 35 -- 31, 35 -- 36, 36 -- 31, 36 -- 32, 24 -- 37, 37 -- 38, 38 -- 39, 39 -- 40, 40 -- 25, 40 -- 26, 23 -- 37,
      40 -- 41, 41 -- 26, 41 -- 42, 42 -- 43, 43 -- 44, 44 -- 12, 44 -- 13, 39 -- 45, 45 -- 40, 45 -- 41, 45 -- 46, 46 -- 41, 46 -- 42, 29 -- 47,
      47 -- 48, 48 -- 49, 49 -- 50, 50 -- 35, 50 -- 36, 28 -- 47
    )
  val t: Tiling =
    Tiling.maybe(edges).unsafe

  "Another tiling" can "have its nodes grouped by vertex in uniform sets" in {
    t.groupUniformsNestedComplete shouldBe
      Map(
        Vector(3, 3, 3, 3, 6) -> List((List(), Vector(5, 10, 14, 1, 2, 7, 11, 8, 4, 37, 25, 20, 28, 21, 13, 41, 17, 34, 22, 44, 27, 3, 35, 50, 16, 31, 40, 26, 23, 36, 30, 19, 47))),
        Vector(3, 6, 3, 6) -> List((List(), Vector(6, 9, 12, 18, 24, 29, 32, 15)))
      ).mapValues2(_.map((list, nodes) => (list, nodes.map(Node(_)))))
  }

  it can "have uniformity value 2" in {
    t.uniformity shouldBe
      2 // should be 3
  }

  "A uniform5v2 [2x(3₃.4₂);2x(3.4₂.6);(3.6.3.6)]" can "have uniformity value 5" in {
    uniform5v2.uniformity shouldBe
      5
  }

  "Some 2-uniform outlier" can "be examined" in {
    val outlier: Tiling =
      Tiling.maybe(
        1--2, 1--3, 1--4, 1--5, 1--6, 1--7, 2--3, 2--7, 2--9, 3--4, 3--8, 4--5, 4--21, 5--6, 5--42, 6--7, 6--19, 7--18,
        8--9, 8--20, 8--29, 9--10, 9--20, 10--11, 10--20, 10--30, 11--12, 11--31, 11--32, 12--13, 12--32, 12--33,
        13--14, 13--34, 13--35, 14--15, 14--35, 14--36, 15--16, 15--37, 15--38, 16--17, 16--38, 16--39, 17--18,
        17--40, 17--41, 18--19, 18--41, 19--41, 19--46, 20--29, 20--30, 20--44, 21--22, 21--42, 21--43, 22--23,
        22--43, 22--62, 23--24, 23--61, 23--63, 24--25, 24--63, 24--65, 25--26, 25--64, 25--66, 26--27, 26--66,
        26--68, 27--28, 27--67, 27--69, 28--29, 28--45, 28--69, 29--44, 30--31, 30--44, 31--32, 31--54, 32--33,
        32--54, 32--55, 33--34, 33--55, 34--35, 34--56, 35--36, 35--56, 35--57, 36--37, 36--57, 37--38, 37--58,
        38--39, 38--58, 38--59, 39--40, 39--59, 40--41, 40--60, 41--46, 41--60, 42--43, 42--53, 43--53, 43--62,
        43--101, 44--45, 45--69, 45--76, 46--47, 46--60, 47--48, 47--93, 47--149, 48--49, 48--149, 48--151, 49--50,
        49--150, 49--152, 50--51, 50--152, 50--154, 51--52, 51--153, 51--155, 52--53, 52--102, 52--155, 53--101,
        54--55, 54--70, 55--84, 56--57, 56--77, 57--92, 58--59, 58--85, 59--100, 60--93, 61--62, 61--63, 61--103,
        62--101, 63--65, 63--103, 63--111, 64--65, 64--66, 64--113, 65--111, 66--68, 66--112, 66--113, 67--68, 67--69,
        67--114, 68--112, 69--76, 69--114, 70--71, 70--84, 70--122, 71--72, 71--120, 71--122, 72--73, 72--119,
        72--121, 73--74, 73--117, 73--119, 74--75, 74--116, 74--118, 75--76, 75--115, 75--116, 76--114, 77--78,
        77--92, 77--131, 78--79, 78--129, 78--131, 79--80, 79--128, 79--130, 80--81, 80--126, 80--128, 81--82, 81--125,
        81--127, 82--83, 82--123, 82--125, 83--84, 83--122, 83--124, 84--122, 85--86, 85--100, 85--140, 86--87, 86--138,
        86--140, 87--88, 87--137, 87--139, 88--89, 88--135, 88--137, 89--90, 89--134, 89--136, 90--91, 90--132, 90--134,
        91--92, 91--131, 91--133, 92--131, 93--94, 93--149, 94--95, 94--147, 94--149, 95--96, 95--146, 95--148, 96--97,
        96--144, 96--146, 97--98, 97--143, 97--145, 98--99, 98--141, 98--143, 99--100, 99--140, 99--142, 100--140,
        101--102, 102--110, 102--155, 103--104, 103--111, 104--105, 105--106, 106--107, 107--108, 108--109, 109--110,
        110--155, 111--156, 112--113, 112--164, 113--163, 114--115, 115--116, 115--170, 116--118, 117--118, 117--119,
        119--121, 120--121, 120--122, 122--124, 123--124, 123--125, 125--127, 126--127, 126--128, 128--130, 129--130,
        129--131, 131--133, 132--133, 132--134, 134--136, 135--136, 135--137, 137--139, 138--139, 138--140, 140--142,
        141--142, 141--143, 143--145, 144--145, 144--146, 146--148, 147--148, 147--149, 149--151, 150--151, 150--152,
        152--154, 153--154, 153--155, 156--157, 157--158, 158--159, 159--160, 160--161, 161--162, 162--163, 164--165,
        165--166, 166--167, 167--168, 168--169, 169--170
      ).unsafe
    outlier.uniformity shouldBe
      2
  }

  "An uniformityIssue" can "have uniformity value 2" in {
    uniformityIssue.uniformity shouldBe
      2
  }

  "An uniformityIssue2" can "have uniformity value 2" in {
    uniformityIssue2.uniformity shouldBe
      2
  }

  "An uniformityIssue3" can "have uniformity value 2" in {
    uniformityIssue3.uniformity shouldBe
      2
  }

  "An uniformityIssue4" can "have uniformity value 2" in {
    uniformityIssue4.uniformity shouldBe
      2
  }

  "An uniformityIssue5" can "have uniformity value 2" in {
    uniformityIssue5.uniformity shouldBe
      2
  }

  "A 2-uniform thing" can "have uniformity value 2" in {
    val t2: Tiling =
      Tiling.maybe(
        2--8, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 1--7, 1--5, 1--4, 1--3, 1--2, 4--9, 9--10, 3--10, 10--11, 3--11,
        11--12, 2--12, 8--13, 2--13, 12--13, 8--14, 7--14, 14--15, 7--15, 6--15, 13--16, 8--16, 16--17, 14--17, 12--18,
        18--19, 13--19, 19--20, 16--20, 11--21, 12--21, 18--21, 20--22, 16--22, 17--22, 18--23, 19--23, 23--24, 19--24,
        20--24, 10--25, 11--25, 25--26, 21--26, 21--27, 18--27, 26--28, 27--28, 27--29, 23--29, 28--30, 27--30, 29--30,
        25--31, 26--31, 31--32, 26--32, 28--32, 10--33, 25--33, 9--34, 33--34, 33--35, 31--35, 34--36, 33--36, 35--36,
        4--37, 9--37, 5--38, 37--38, 6--39, 5--39, 38--39, 37--40, 9--40, 34--40, 15--41, 41--42, 6--42, 39--42, 38--43,
        43--44, 39--44, 44--45, 42--45, 37--46, 38--46, 43--46, 40--47, 46--47, 45--48, 42--48, 41--48, 43--49, 44--49,
        49--50, 44--50, 45--50, 22--51, 20--51, 51--52, 24--52, 17--53, 53--54, 22--54, 54--55, 51--55, 14--56, 17--56,
        53--56, 15--57, 56--57
      ).unsafe
    t2.uniformity shouldBe
      2
  }

  "A 3-uniform 2-Archimedean tiling [2x(3₆);(3₄.6)]" can "be described" in {
    val t: Tiling =
      uniform3gonal2
    (t.gonality, t.uniformity) shouldBe
      (2, 3)
  }

  "A 4-uniform 4-Archimedean tiling [(3₆);(3₄.6);(3₂.6₂);(6₃)]" can "be described" in {
    val t: Tiling =
      uniform4gonal4
    (t.gonality, t.uniformity) shouldBe
      (4, 4)
  }

  "Another x-uniform pattern" can "be described" in {
    val t: Tiling =
      uniform6Other
    t.uniformity shouldBe
      6 // looks like it should be higher
  }

  "A 3-uniform pattern" can "NOT be detected, at this early stage" in {
    val edges: List[Edge] =
      List(
        9--2, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 1--6, 1--5, 1--4, 1--3, 1--2, 2--10, 10--9, 10--11, 11--12,
        12--13, 13--14, 14--9, 14--8, 4--15, 15--16, 16--17, 17--18, 18--3, 18--19, 19--3, 19--2, 10--19, 5--20, 20--4,
        20--15, 19--21, 21--10, 21--11, 18--22, 22--23, 23--24, 24--21, 17--22, 24--25, 25--21, 25--11, 25--26, 26--11,
        26--12, 6--27, 27--28, 28--29, 29--30, 30--5, 30--20, 7--27, 30--31, 31--20, 31--32, 32--33, 33--34, 34--15,
        34--16, 29--35, 35--30, 35--31, 35--36, 36--31, 36--32, 24--37, 37--38, 38--39, 39--40, 40--25, 40--26, 23--37,
        40--41, 41--26, 41--42, 42--43, 43--44, 44--12, 44--13, 39--45, 45--40, 45--41, 45--46, 46--41, 46--42, 29--47,
        47--48, 48--49, 49--50, 50--35, 50--36, 28--47)
    val t: Tiling =
      Tiling.maybe(edges).unsafe
    t.uniformity shouldBe
      2 // should be 3
  }

  "A complex tiling [3x(3.4.3.12);2x(3.4.6.4);2x(3.12₂)]" must "be described" in {
    uniformityProblem.uniformity shouldBe
      7
  }

  it must "have 85 polygons" in {
    uniformityProblem.countPolygons shouldBe
      85
  }

  val layers: List[List[List[Edge]]] =
    uniformityProblem.outerPolygonsFromSingle(Node(15))

  it must "have 85 polygons path found in different layers while searching for uniformity" in {
    layers.flatten.size shouldBe
      85
  }

  it can "be examined for outer polygons from node 15" in {
    layers shouldBe
      List(
        List(
          List(20--34, 34--35, 35--36, 27--36, 15--27, 15--20), List(24--27, 6--24, 6--15, 15--27),
          List(5--15, 5--17, 17--20, 15--20), List(5--6, 5--15, 6--15)
        ),
        List(
          List(34--126, 106--126, 35--106, 34--35), List(35--95, 76--95, 36--76, 35--36),
          List(20--34, 20--37, 37--62, 34--62), List(17--20, 17--37, 20--37), List(53--66, 27--53, 27--36, 36--66),
          List(24--53, 24--27, 27--53), List(4--5, 4--17, 5--17),
          List(5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 11--12, 1--12, 1--2, 2--3, 3--4, 4--5), List(6--7, 6--24, 7--24)
        ),
        List(
          List(37--38, 38--62, 37--62),
          List(42--43, 41--42, 40--41, 39--40, 38--39, 37--38, 17--37, 4--17, 3--4, 3--22, 22--44, 43--44),
          List(107--126, 106--107, 106--126), List(34--62, 62--126, 34--126), List(95--106, 35--95, 35--106),
          List(66--76, 36--66, 36--76), List(2--22, 3--22, 2--3), List(1--13, 2--13, 1--2), List(1--16, 1--12, 12--16),
          List(10--23, 11--23, 10--11), List(9--14, 10--14, 9--10), List(8--18, 9--18, 8--9),
          List(16--52, 12--16, 11--12, 11--23, 23--45, 45--46, 46--47, 47--48, 48--49, 49--50, 50--51, 51--52),
          List(54--66, 53--54, 53--66),
          List(54--55, 55--56, 56--57, 57--58, 58--59, 59--60, 18--60, 8--18, 7--8, 7--24, 24--53, 53--54),
          List(77--95, 76--77, 76--95)
        ),
        List(
          List(40--132, 40--41, 41--75, 74--75, 74--150, 149--150, 148--149, 147--148, 146--147, 145--146, 131--145, 131--132),
          List(43--64, 43--44, 44--64), List(42--75, 41--75, 41--42),
          List(68--69, 69--70, 70--71, 71--72, 72--73, 73--74, 74--75, 42--75, 42--43, 43--64, 64--67, 67--68),
          List(107--108, 108--109, 109--110, 110--111, 111--112, 112--113, 113--114, 78--114, 77--78, 77--95, 95--106, 106--107),
          List(40--132, 39--132, 39--40),
          List(107--108, 107--126, 62--126, 38--62, 38--39, 39--132, 131--132, 130--131, 129--130, 128--129, 127--128, 108--127),
          List(49--138, 50--138, 49--50),
          List(98--99, 98--124, 61--124, 51--61, 50--51, 50--138, 137--138, 136--137, 135--136, 134--135, 133--134, 99--133),
          List(92--156, 155--156, 154--155, 153--154, 152--153, 151--152, 137--151, 137--138, 49--138, 48--49, 48--93, 92--93),
          List(2--13, 13--25, 22--25, 2--22),
          List(25--44, 22--44, 22--25), List(1--13, 1--16, 16--19, 13--19), List(16--19, 16--52, 19--52),
          List(10--14, 14--26, 23--26, 10--23), List(18--21, 14--21, 9--14, 9--18), List(26--45, 23--45, 23--26),
          List(18--60, 21--60, 18--21), List(51--52, 51--61, 52--61), List(46--65, 45--46, 45--65),
          List(59--60, 59--63, 60--63), List(55--84, 56--84, 55--56),
          List(77--78, 78--79, 79--80, 80--81, 81--82, 82--83, 83--84, 55--84, 54--55, 54--66, 66--76, 76--77),
          List(47--93, 48--93, 47--48),
          List(86--87, 87--88, 88--89, 89--90, 90--91, 91--92, 92--93, 47--93, 46--47, 46--65, 65--85, 85--86),
          List(57--144, 58--144, 57--58),
          List(116--117, 116--125, 63--125, 59--63, 58--59, 58--144, 143--144, 142--143, 141--142, 140--141, 139--140, 117--139),
          List(83--157, 157--158, 158--159, 159--160, 160--161, 161--162, 143--162, 143--144, 57--144, 56--57, 56--84, 83--84)
        ),
        List(
          List(68--94, 67--68, 67--94), List(98--124, 97--98, 97--124), List(69--105, 70--105, 69--70),
          List(98--99, 99--100, 100--101, 101--102, 102--103, 103--104, 104--105, 69--105, 68--69, 68--94, 94--97, 97--98),
          List(78--114, 79--114, 78--79), List(108--127, 109--127, 108--109), List(99--133, 100--133, 99--100),
          List(13--19, 19--30, 29--30, 28--29, 25--28, 13--25), List(25--28, 28--64, 44--64, 25--44),
          List(28--67, 64--67, 28--64), List(30--61, 61--124, 30--124), List(19--52, 52--61, 30--61, 19--30),
          List(14--21, 21--31, 31--32, 32--33, 26--33, 14--26), List(21--60, 60--63, 31--63, 21--31),
          List(26--33, 33--65, 45--65, 26--45), List(33--85, 65--85, 33--65), List(63--125, 31--125, 31--63),
          List(86--96, 85--86, 85--96), List(87--123, 88--123, 87--88), List(116--125, 115--116, 115--125),
          List(86--87, 86--96, 96--115, 115--116, 116--117, 117--118, 118--119, 119--120, 120--121, 121--122, 122--123, 87--123),
          List(117--139, 118--139, 117--118)
        ),
        List(
          List(28--29, 29--94, 67--94, 28--67), List(29--97, 94--97, 29--94), List(30--124, 97--124, 29--97, 29--30),
          List(32--96, 85--96, 33--85, 32--33), List(31--125, 115--125, 32--115, 31--32), List(96--115, 32--96, 32--115)
        )
      )
  }

  it can "be examined alternatively for perimeter nodes from node 15" in {
    uniformityProblem.outerOrderedPolygonsFromSingle(Node(15))/*.map((a, b) => (a.map(_.map(_.stringify)), b))*/ shouldBe
      List(
        (List(
          List(5--15, 5--17, 17--20, 15--20), List(20--34, 34--35, 35--36, 27--36, 15--27, 15--20),
          List(24--27, 6--24, 6--15, 15--27), List(5--6, 5--15, 6--15)), true),
        (List(
          List(4--5, 4--17, 5--17), List(17--20, 17--37, 20--37), List(20--34, 20--37, 37--62, 34--62),
          List(34--126, 106--126, 35--106, 34--35), List(35--95, 76--95, 36--76, 35--36),
          List(53--66, 27--53, 27--36, 36--66), List(24--53, 24--27, 27--53), List(6--7, 6--24, 7--24),
          List(5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 11--12, 1--12, 1--2, 2--3, 3--4, 4--5)), true),
        (List(
          List(2--22, 3--22, 2--3), List(1--13, 2--13, 1--2), List(1--16, 1--12, 12--16),
          List(16--52, 12--16, 11--12, 11--23, 23--45, 45--46, 46--47, 47--48, 48--49, 49--50, 50--51, 51--52),
          List(10--23, 11--23, 10--11), List(9--14, 10--14, 9--10), List(8--18, 9--18, 8--9),
          List(54--55, 55--56, 56--57, 57--58, 58--59, 59--60, 18--60, 8--18, 7--8, 7--24, 24--53, 53--54),
          List(54--55, 55--56, 56--57, 57--58, 58--59, 59--60, 18--60, 8--18, 7--8, 7--24, 24--53, 53--54),
          List(54--55, 55--56, 56--57, 57--58, 58--59, 59--60, 18--60, 8--18, 7--8, 7--24, 24--53, 53--54),
          List(54--66, 53--54, 53--66), List(66--76, 36--66, 36--76), List(66--76, 36--66, 36--76),
          List(77--95, 76--77, 76--95), List(95--106, 35--95, 35--106), List(95--106, 35--95, 35--106),
          List(107--126, 106--107, 106--126), List(34--62, 62--126, 34--126), List(34--62, 62--126, 34--126),
          List(37--38, 38--62, 37--62),
          List(42--43, 41--42, 40--41, 39--40, 38--39, 37--38, 17--37, 4--17, 3--4, 3--22, 22--44, 43--44),
          List(42--43, 41--42, 40--41, 39--40, 38--39, 37--38, 17--37, 4--17, 3--4, 3--22, 22--44, 43--44),
          List(42--43, 41--42, 40--41, 39--40, 38--39, 37--38, 17--37, 4--17, 3--4, 3--22, 22--44, 43--44)), true),
        (List(
          List(18--21, 14--21, 9--14, 9--18), List(18--21, 14--21, 9--14, 9--18), List(10--14, 14--26, 23--26, 10--23),
          List(10--14, 14--26, 23--26, 10--23), List(26--45, 23--45, 23--26), List(46--65, 45--46, 45--65),
          List(86--87, 87--88, 88--89, 89--90, 90--91, 91--92, 92--93, 47--93, 46--47, 46--65, 65--85, 85--86),
          List(47--93, 48--93, 47--48),
          List(92--156, 155--156, 154--155, 153--154, 152--153, 151--152,
            137--151, 137--138, 49--138, 48--49, 48--93, 92--93),
          List(49--138, 50--138, 49--50),
          List(98--99, 98--124, 61--124, 51--61, 50--51, 50--138, 137--138,
            136--137, 135--136, 134--135, 133--134, 99--133),
          List(51--52, 51--61, 52--61), List(16--19, 16--52, 19--52), List(1--13, 1--16, 16--19, 13--19),
          List(1--13, 1--16, 16--19, 13--19), List(2--13, 13--25, 22--25, 2--22), List(2--13, 13--25, 22--25, 2--22),
          List(25--44, 22--44, 22--25), List(43--64, 43--44, 44--64),
          List(68--69, 69--70, 70--71, 71--72, 72--73, 73--74, 74--75, 42--75, 42--43, 43--64, 64--67, 67--68),
          List(42--75, 41--75, 41--42),
          List(40--132, 40--41, 41--75, 74--75, 74--150, 149--150, 148--149,
            147--148, 146--147, 145--146, 131--145, 131--132),
          List(40--132, 39--132, 39--40),
          List(107--108, 107--126, 62--126, 38--62, 38--39, 39--132, 131--132,
            130--131, 129--130, 128--129, 127--128, 108--127),
          List(107--108, 107--126, 62--126, 38--62, 38--39, 39--132, 131--132,
            130--131, 129--130, 128--129, 127--128, 108--127),
          List(107--108, 107--126, 62--126, 38--62, 38--39, 39--132, 131--132,
            130--131, 129--130, 128--129, 127--128, 108--127),
          List(107--108, 107--126, 62--126, 38--62, 38--39, 39--132, 131--132,
            130--131, 129--130, 128--129, 127--128, 108--127),
          List(107--108, 108--109, 109--110, 110--111, 111--112, 112--113,
            113--114, 78--114, 77--78, 77--95, 95--106, 106--107),
          List(107--108, 108--109, 109--110, 110--111, 111--112, 112--113,
            113--114, 78--114, 77--78, 77--95, 95--106, 106--107),
          List(107--108, 108--109, 109--110, 110--111, 111--112, 112--113,
            113--114, 78--114, 77--78, 77--95, 95--106, 106--107),
          List(77--78, 78--79, 79--80, 80--81, 81--82, 82--83, 83--84, 55--84, 54--55, 54--66, 66--76, 76--77),
          List(77--78, 78--79, 79--80, 80--81, 81--82, 82--83, 83--84, 55--84, 54--55, 54--66, 66--76, 76--77),
          List(77--78, 78--79, 79--80, 80--81, 81--82, 82--83, 83--84, 55--84, 54--55, 54--66, 66--76, 76--77),
          List(77--78, 78--79, 79--80, 80--81, 81--82, 82--83, 83--84, 55--84, 54--55, 54--66, 66--76, 76--77),
          List(55--84, 56--84, 55--56),
          List(83--157, 157--158, 158--159, 159--160, 160--161, 161--162,
            143--162, 143--144, 57--144, 56--57, 56--84, 83--84),
          List(57--144, 58--144, 57--58),
          List(116--117, 116--125, 63--125, 59--63, 58--59, 58--144,
            143--144, 142--143, 141--142, 140--141, 139--140, 117--139),
          List(59--60, 59--63, 60--63), List(18--60, 21--60, 18--21)), true),
        (List(
          List(116--125, 115--116, 115--125), List(63--125, 31--125, 31--63), List(21--60, 60--63, 31--63, 21--31),
          List(21--60, 60--63, 31--63, 21--31), List(14--21, 21--31, 31--32, 32--33, 26--33, 14--26),
          List(14--21, 21--31, 31--32, 32--33, 26--33, 14--26), List(26--33, 33--65, 45--65, 26--45),
          List(26--33, 33--65, 45--65, 26--45), List(33--85, 65--85, 33--65), List(86--96, 85--86, 85--96)), false),
        (List(), false)
      )
  }

  it can "be examined for outer polygon strips from node 15" in {
    uniformityProblem.outerOrderedStripFromSingle(Node(15)) shouldBe
      List(
        (List(4, 6, 4, 3), true),
        (List(3, 3, 4, 4, 4, 4, 3, 3, 12), true),
        (List(3, 3, 3, 12, 3, 3, 3, 12, 12, 12, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 12, 12, 12), true),
        (List(4, 4, 4, 4, 3, 3, 12, 3, 12, 3, 12, 3, 3, 4, 4, 4, 4, 3, 3, 12, 3, 12,
          3, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 3, 12, 3, 12, 3, 3), true),
        (List(3, 3, 4, 4, 6, 6, 4, 4, 3, 3), false),
        (List(), false)
      )
  }

  val matrixProblem: Map[Node, List[(List[Int], Boolean)]] =
    uniformityProblem.outerOrderedStripFrom(uniformityProblem.graphEdges.nodes.diff(uniformityProblem.perimeter.toRingNodes))

//  "A uniformityProblem" can "return an uniform nodes tree" in {
//    uniformityProblem.uniformNodesTree(matrixProblem) shouldBe
//      Map(
//        List(2, 0) -> List(14, 13, 15),
//        List(1, 0) -> List(68, 47, 58, 55, 40, 87, 99, 98, 39, 86, 49, 116, 78, 57, 69, 107, 50, 48, 108, 41, 77, 117, 42, 56, 12, 7, 3, 11, 8, 4),
//        List(2, 1) -> List(25, 20, 21, 27, 26, 19),
//        List(0, 0) -> List(37, 52, 60, 53, 45, 44),
//        List(0, 1) -> List(24, 17, 22, 18, 16, 23),
//        List(1, 1) -> List(75, 144, 54, 132, 93, 46, 138, 51, 43, 59, 38, 84),
//        List(0, 2) -> List(5, 10, 1, 6, 9, 2)
//      )
//  }

  it can "return a group uniforms" in {
    uniformityProblem.groupUniforms shouldBe
      Map(
        (Vertex(3, 4, 3, 12), List(0)) -> Vector(37, 52, 60, 53, 45, 44),
        (Vertex(3, 4, 6, 4), List(0)) -> Vector(14, 13, 15),
        (Vertex(3, 12, 12), List(1)) -> Vector(75, 144, 54, 132, 93, 46, 138, 51, 43, 59, 38, 84),
        (Vertex(3, 12, 12), List(0)) -> Vector(68, 47, 58, 55, 40, 87, 99, 98, 39, 86, 49, 116, 78, 57, 69, 107, 50, 48, 108, 41, 77, 117, 42, 56, 12, 7, 3, 11, 8, 4),
        (Vertex(3, 4, 3, 12), List(2)) -> Vector(5, 10, 1, 6, 9, 2),
        (Vertex(3, 4, 6, 4), List(1)) -> Vector(25, 20, 21, 27, 26, 19),
        (Vertex(3, 4, 3, 12), List(1)) -> Vector(24, 17, 22, 18, 16, 23)
      )
  }

  "Another smaller tiling [(3.4.6.4)]" can "be examined for outer polygon strips from perimeter node 22" in {
    triSqrHexHexoid.outerOrderedStripFromSingle(Node(22)) shouldBe
      List(
        (List(6), false),
        (List(4), false),
        (List(3, 3, 6, 3, 3), false),
        (List(4, 4, 4, 4, 4, 4, 4, 4, 4), false),
        (List(), false),
        (List(), false)
      )
  }

  val matrix: Map[Node, List[(List[Int], Boolean)]] =
    triSqrHexHexoid.outerOrderedStripFrom(triSqrHexHexoid.graphEdges.nodes.diff(triSqrHexHexoid.perimeter.toRingNodes))

  it can "be examined for outer polygon strips from all nodes" in {
    matrix shouldBe
      Map(
        5 -> List(
          (List(4, 6, 4, 3), true), (List(6, 3, 4, 4, 4, 4, 3, 6, 4), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        10 -> List(
          (List(3, 4, 6, 4), true), (List(4, 6, 3), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        14 -> List(
          (List(6, 4, 3, 4), true), (List(4, 6, 3), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        1 -> List(
          (List(6, 4, 3, 4), true), (List(4, 4, 4, 3, 6, 4, 6, 3, 4), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        6 -> List(
          (List(6, 4, 3, 4), true), (List(4, 4, 3, 6, 4, 6, 3, 4, 4), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        9 -> List(
          (List(4, 6, 4, 3), true), (List(4, 6, 3), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        13 -> List(
          (List(3, 4, 6, 4), true), (List(4, 6, 3), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        2 -> List(
          (List(6, 4, 3, 4), true), (List(4, 4, 3, 6, 4, 6, 3, 4, 4), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        17 -> List(
          (List(4, 3, 4, 6), true), (List(3, 6, 4), false), (List(3, 4, 4, 4, 4, 4), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        12 -> List(
          (List(4, 3, 4, 6), true), (List(4, 6, 3), false), (List(3, 4, 4, 4, 4, 4), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)), 7 -> List((List(4, 6, 4, 3), true),
          (List(4, 6, 3), false), (List(3, 4, 4, 4, 4, 4), false), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        3 -> List(
          (List(6, 4, 3, 4), true), (List(4, 3, 6, 4, 6, 3, 4, 4, 4), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        18 -> List(
          (List(4, 6, 4, 3), true), (List(3, 6, 4), false), (List(3, 4, 4, 4, 4, 4), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)), 16 -> List((List(6, 4, 3, 4), true),
          (List(3, 6, 4), false), (List(3, 4, 4, 4, 4, 4), false), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        11 -> List(
          (List(4, 3, 4, 6), true), (List(3, 6, 4), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        8 -> List(
          (List(4, 3, 4, 6), true), (List(3, 6, 4), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false)),
        4 -> List(
          (List(6, 4, 3, 4), true), (List(4, 4, 4, 4, 3, 6, 4, 6, 3), true), (List(3, 3, 6, 3, 3, 6, 3, 3), false),
          (List(), false)),
        15 -> List(
          (List(4, 3, 4, 6), true), (List(4, 6, 3), false), (List(4, 4, 4, 4, 4, 3), false),
          (List(3, 3, 6, 3, 3, 6, 3, 3), false), (List(), false))
      ).map((k, v) => Node(k) -> v)
  }

//  it can "return an uniform nodes tree" in {
//    triSqrHexHexoid.uniformNodesTree(matrix) shouldBe
//      Map(List() -> List(5, 10, 14, 1, 6, 9, 13, 2, 17, 12, 7, 3, 18, 16, 11, 8, 4, 15))
//  }

}

