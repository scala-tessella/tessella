package io.github.scala_tessella.tessella

import Outliers.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GraphPolygonsSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A sqr4x4Reticulate" can "be divided in oriented polygons" in {
    p4444_4by4_reticulate.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(17, 16, 21, 22), Vector(12, 7, 6, 11), Vector(7, 2, 1, 6), Vector(17, 22, 23, 18),
          Vector(12, 11, 16, 17), Vector(13, 8, 7, 12), Vector(8, 3, 2, 7), Vector(23, 24, 19, 18),
          Vector(24, 25, 20, 19), Vector(12, 17, 18, 13), Vector(18, 19, 14, 13), Vector(14, 9, 8, 13),
          Vector(9, 4, 3, 8), Vector(19, 20, 15, 14), Vector(15, 10, 9, 14), Vector(10, 5, 4, 9)
        )
      )
  }

  "A sqr3x3Reticulate" can "be divided in unoriented polygons" in {
    p4444_3by3_grown.tilingUnorientedPolygons shouldBe
      Option(
        List(
          Vector(1, 4, 3, 2), Vector(4, 5, 6, 1), Vector(6, 7, 8, 1), Vector(3, 12, 11, 2), Vector(5, 16, 15, 4),
          Vector(15, 14, 3, 4), Vector(14, 13, 12, 3), Vector(8, 9, 2, 1), Vector(11, 10, 9, 2)
        )
      )
  }

  it can "have listed all the simple cycles with maximum lenght 4 nodes" in {
    p4444_3by3_grown.findSimpleCycles(4) shouldBe
      List(
        Vector(2, 3, 12, 11), Vector(1, 2, 3, 4), Vector(2, 9, 10, 11),
        Vector(1, 4, 5, 6), Vector(1, 6, 7, 8), Vector(4, 5, 16, 15),
        Vector(3, 12, 13, 14), Vector(1, 2, 9, 8), Vector(3, 4, 15, 14)
      )
  }

  it can "have listed all the simple cycles with maximum lenght 6 nodes" in {
    p4444_3by3_grown.findSimpleCycles(6) shouldBe
      List(
        Vector(1, 2, 11, 12, 3, 4), Vector(1, 4, 5, 6, 7, 8), Vector(1, 4, 5, 6), Vector(1, 4, 15, 16, 5, 6),
        Vector(1, 4, 3, 2, 9, 8), Vector(4, 5, 16, 15), Vector(1, 2, 9, 8), Vector(3, 4, 15, 14),
        Vector(2, 3, 14, 13, 12, 11), Vector(3, 4, 5, 16, 15, 14), Vector(2, 3, 12, 11, 10, 9),
        Vector(1, 2, 3, 14, 15, 4), Vector(2, 3, 12, 11), Vector(1, 2, 3, 4), Vector(1, 2, 9, 8, 7, 6),
        Vector(2, 9, 10, 11), Vector(3, 4, 15, 14, 13, 12), Vector(1, 6, 7, 8), Vector(1, 2, 11, 10, 9, 8),
        Vector(3, 12, 13, 14), Vector(1, 2, 3, 4, 5, 6)
      )
  }

  it can "have listed all the unoriented polygons with an alt method" in {
    p4444_3by3_grown.tilingUnorientedPolygonsAlt(12) shouldBe
      List(
        Vector(1, 6, 7, 8), Vector(1, 4, 5, 6), Vector(4, 5, 16, 15),
        Vector(2, 9, 10, 11), Vector(3, 4, 15, 14), Vector(2, 3, 12, 11),
        Vector(1, 2, 3, 4), Vector(3, 12, 13, 14), Vector(1, 2, 9, 8)
      )
  }

  "A triHexOfSide3" can "be divided in oriented polygons" in {
    p333333_grown_hexagon.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(12, 26, 27), Vector(13, 27, 28), Vector(28, 29, 13), Vector(11, 24, 25), Vector(25, 26, 11),
          Vector(11, 26, 12), Vector(27, 13, 12), Vector(14, 13, 29), Vector(29, 30, 14), Vector(24, 11, 10),
          Vector(10, 23, 24), Vector(33, 34, 17), Vector(15, 31, 32), Vector(15, 30, 31), Vector(14, 30, 15),
          Vector(4, 12, 13), Vector(13, 14, 4), Vector(3, 11, 12), Vector(10, 11, 3), Vector(9, 10, 2),
          Vector(9, 23, 10), Vector(9, 22, 23), Vector(21, 22, 9), Vector(17, 34, 35), Vector(17, 16, 33),
          Vector(16, 32, 33), Vector(16, 15, 32), Vector(5, 15, 16), Vector(15, 5, 14), Vector(4, 14, 5),
          Vector(3, 12, 4), Vector(3, 2, 10), Vector(2, 8, 9), Vector(9, 8, 21), Vector(20, 21, 8), Vector(35, 36, 18),
          Vector(36, 19, 18), Vector(19, 7, 18), Vector(18, 17, 35), Vector(18, 7, 6), Vector(6, 17, 18),
          Vector(6, 16, 17), Vector(16, 6, 5), Vector(1, 5, 6), Vector(6, 7, 1), Vector(5, 1, 4), Vector(1, 3, 4),
          Vector(2, 3, 1), Vector(1, 7, 2), Vector(8, 2, 7), Vector(7, 19, 8), Vector(8, 19, 20), Vector(37, 20, 19),
          Vector(19, 36, 37))
      )
  }

  "A gonExperiment" can "be divided in oriented polygons" in {
    gonExperiment.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(5, 23, 22), Vector(5, 22, 16), Vector(4, 5, 16, 15), Vector(9, 20, 19, 18, 17, 8),
          Vector(15, 29, 28, 27, 26, 25, 24, 14), Vector(11, 21, 10), Vector(4, 1, 6, 5), Vector(8, 7, 6, 1),
          Vector(1, 4, 3, 2), Vector(2, 9, 8, 1), Vector(2, 11, 10, 9), Vector(4, 15, 14, 3), Vector(2, 3, 12, 11),
          Vector(14, 13, 12, 3)
        )
      )
  }

  "A hex4x4Reticulate" can "be divided in oriented polygons" in {
    p666_4by4_reticulate.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(37, 46, 47, 48, 39, 38), Vector(37, 36, 35, 44, 45, 46), Vector(32, 31, 40, 41, 42, 33),
          Vector(33, 42, 43, 44, 35, 34), Vector(25, 34, 35, 36, 27, 26), Vector(36, 37, 38, 29, 28, 27),
          Vector(32, 23, 22, 21, 30, 31), Vector(28, 19, 18, 17, 26, 27), Vector(11, 20, 21, 22, 13, 12),
          Vector(11, 12, 3, 2, 1, 10), Vector(12, 13, 14, 5, 4, 3), Vector(13, 22, 23, 24, 15, 14),
          Vector(23, 32, 33, 34, 25, 24), Vector(24, 25, 26, 17, 16, 15), Vector(17, 18, 9, 8, 7, 16),
          Vector(7, 6, 5, 14, 15, 16)
        )
      )
  }

  "A differentOrientations" must "be divided in oriented polygons" in {
    differentOrientations.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(16, 15, 5), Vector(5, 15, 14), Vector(4, 14, 13), Vector(6, 16, 5), Vector(14, 4, 5),
          Vector(13, 12, 4), Vector(19, 16, 6), Vector(5, 1, 6), Vector(5, 4, 1), Vector(1, 4, 3), Vector(4, 12, 3),
          Vector(12, 11, 3), Vector(3, 11, 9), Vector(7, 6, 1), Vector(17, 19, 6), Vector(3, 2, 1), Vector(9, 2, 3),
          Vector(2, 9, 10), Vector(10, 21, 8), Vector(17, 6, 7), Vector(1, 2, 7), Vector(10, 8, 2), Vector(18, 17, 7),
          Vector(7, 2, 8), Vector(8, 18, 7), Vector(8, 20, 18)
        )
      )
  }

  "A triangleSquareInserted" can "be divided in oriented polygons" in {
    triangleSquareInserted.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(28, 58, 56, 15), Vector(26, 33, 35, 28), Vector(13, 26, 28, 15), Vector(15, 56, 55, 9),
          Vector(23, 25, 48, 47), Vector(27, 34, 33, 26), Vector(10, 54, 53, 11), Vector(11, 53, 57, 25),
          Vector(47, 52, 51, 46), Vector(9, 55, 54, 10), Vector(13, 15, 9, 8), Vector(24, 23, 47, 46),
          Vector(46, 51, 50), Vector(14, 27, 26, 13), Vector(31, 34, 27), Vector(31, 43, 34), Vector(10, 11, 6, 7),
          Vector(11, 25, 23, 6), Vector(31, 32, 44, 43), Vector(32, 40, 45, 44), Vector(8, 9, 10, 7),
          Vector(7, 6, 5, 1), Vector(5, 6, 23, 24), Vector(14, 13, 8, 2), Vector(24, 46, 30), Vector(46, 50, 30),
          Vector(20, 27, 14), Vector(20, 31, 27), Vector(30, 50, 49, 29), Vector(20, 19, 32, 31),
          Vector(39, 40, 32, 19), Vector(8, 7, 1, 2), Vector(1, 3, 2), Vector(3, 12, 2), Vector(12, 14, 2),
          Vector(12, 20, 14), Vector(12, 18, 19, 20), Vector(18, 36, 39, 19), Vector(1, 4, 3), Vector(1, 5, 4),
          Vector(5, 21, 4), Vector(5, 24, 21), Vector(24, 30, 21), Vector(3, 17, 18, 12), Vector(4, 16, 17, 3),
          Vector(21, 22, 16, 4), Vector(30, 29, 22, 21), Vector(18, 17, 37, 36), Vector(17, 16, 38, 37),
          Vector(16, 22, 41, 38), Vector(29, 42, 41, 22)
        )
      )
  }

  "A pentagonGrown" can "be divided in oriented polygons" in {
    pentagonGrown.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(24, 27, 28, 29, 15), Vector(12, 26, 25, 24, 15), Vector(12, 15, 16, 17, 6), Vector(5, 14, 13, 12, 6),
          Vector(2, 3, 4, 5, 1), Vector(5, 6, 7, 8, 1), Vector(8, 9, 10, 11, 1), Vector(8, 20, 19, 18, 9),
          Vector(18, 21, 22, 23, 9), Vector(18, 32, 31, 30, 21)
        )
      )
  }

  "A uniform7gonal3" can "be divided in oriented polygons" in {
    p333333_2x3366_4x666.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(183, 171, 172), Vector(170, 171, 183), Vector(176, 177, 178, 161, 160, 159),
          Vector(123, 124, 107, 106, 105, 122), Vector(158, 159, 160, 143, 142, 141),
          Vector(142, 125, 124, 123, 140, 141), Vector(106, 89, 88, 87, 104, 105), Vector(184, 68, 51),
          Vector(156, 185, 139), Vector(185, 140, 139), Vector(185, 141, 140), Vector(185, 158, 141),
          Vector(185, 156, 157), Vector(157, 158, 185), Vector(183, 172, 155), Vector(183, 153, 170),
          Vector(150, 149, 166, 167, 168, 151), Vector(157, 174, 175, 176, 159, 158),
          Vector(140, 123, 122, 121, 138, 139), Vector(86, 87, 88, 71, 70, 69), Vector(120, 103, 102, 101, 118, 119),
          Vector(103, 104, 87, 86, 85, 102), Vector(122, 105, 104, 103, 120, 121), Vector(153, 152, 151, 168, 169, 170),
          Vector(70, 53, 52, 51, 68, 69), Vector(157, 156, 155, 172, 173, 174), Vector(149, 148, 147, 164, 165, 166),
          Vector(91, 108, 180), Vector(108, 109, 180), Vector(180, 109, 110), Vector(67, 68, 184), Vector(51, 50, 184),
          Vector(179, 18, 19), Vector(179, 1, 18), Vector(2, 1, 179), Vector(179, 3, 2),
          Vector(129, 146, 147, 148, 131, 130), Vector(148, 149, 150, 133, 132, 131),
          Vector(150, 151, 152, 135, 134, 133), Vector(152, 153, 154, 137, 136, 135), Vector(153, 183, 154),
          Vector(183, 155, 154), Vector(155, 156, 139, 138, 137, 154), Vector(109, 126, 127, 128, 111, 110),
          Vector(146, 145, 162, 163, 164, 147), Vector(137, 138, 121, 120, 119, 136), Vector(67, 84, 85, 86, 69, 68),
          Vector(132, 115, 114, 113, 130, 131), Vector(115, 116, 99, 98, 97, 114), Vector(134, 117, 116, 115, 132, 133),
          Vector(119, 118, 117, 134, 135, 136), Vector(118, 101, 100, 99, 116, 117), Vector(101, 102, 85, 84, 83, 100),
          Vector(51, 52, 35, 34, 33, 50), Vector(127, 144, 145, 146, 129, 128), Vector(80, 182, 63),
          Vector(80, 81, 182), Vector(81, 82, 182), Vector(182, 64, 63), Vector(65, 64, 182), Vector(182, 82, 65),
          Vector(92, 180, 93), Vector(92, 91, 180), Vector(110, 93, 180), Vector(82, 83, 84, 67, 66, 65),
          Vector(67, 184, 66), Vector(184, 49, 66), Vector(184, 50, 49), Vector(22, 5, 4, 3, 20, 21),
          Vector(3, 179, 20), Vector(179, 19, 20), Vector(94, 93, 110, 111, 112, 95),
          Vector(111, 128, 129, 130, 113, 112), Vector(57, 56, 55, 72, 73, 74), Vector(82, 81, 98, 99, 100, 83),
          Vector(45, 62, 63, 64, 47, 46), Vector(64, 65, 66, 49, 48, 47), Vector(29, 46, 47, 48, 31, 30),
          Vector(48, 49, 50, 33, 32, 31), Vector(33, 34, 17, 16, 15, 32), Vector(81, 80, 79, 96, 97, 98),
          Vector(113, 114, 97, 96, 95, 112), Vector(56, 39, 38, 37, 54, 55), Vector(15, 14, 13, 30, 31, 32),
          Vector(92, 75, 74, 73, 90, 91), Vector(38, 21, 20, 19, 36, 37), Vector(7, 6, 5, 22, 23, 24),
          Vector(95, 181, 94), Vector(95, 96, 181), Vector(96, 79, 181), Vector(181, 77, 94), Vector(78, 77, 181),
          Vector(181, 79, 78), Vector(79, 80, 63, 62, 61, 78), Vector(77, 76, 75, 92, 93, 94),
          Vector(76, 59, 58, 57, 74, 75), Vector(58, 41, 40, 39, 56, 57), Vector(40, 23, 22, 21, 38, 39),
          Vector(40, 41, 42, 25, 24, 23), Vector(25, 26, 9, 8, 7, 24), Vector(61, 60, 59, 76, 77, 78),
          Vector(60, 43, 42, 41, 58, 59), Vector(62, 45, 44, 43, 60, 61), Vector(44, 27, 26, 25, 42, 43),
          Vector(44, 45, 46, 29, 28, 27), Vector(29, 30, 13, 12, 11, 28), Vector(11, 10, 9, 26, 27, 28)
        )
      )
  }

  "A uniform3gonal2" can "be divided in oriented polygons" in {
    p2x333333_33336.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(1, 2, 49), Vector(49, 10, 1), Vector(38, 39, 48, 47, 46, 37), Vector(55, 5, 6), Vector(6, 7, 55),
          Vector(28, 29, 60), Vector(29, 38, 60), Vector(60, 38, 37), Vector(58, 37, 46), Vector(46, 45, 58),
          Vector(53, 3, 4), Vector(4, 5, 53), Vector(49, 2, 3), Vector(11, 10, 49), Vector(51, 30, 21),
          Vector(56, 16, 17), Vector(17, 26, 56), Vector(16, 59, 17), Vector(16, 55, 7), Vector(7, 59, 16),
          Vector(50, 12, 13), Vector(13, 12, 53), Vector(53, 14, 13), Vector(14, 53, 5), Vector(5, 55, 14),
          Vector(27, 60, 36), Vector(27, 28, 60), Vector(37, 36, 60), Vector(36, 37, 58), Vector(58, 45, 44),
          Vector(12, 3, 53), Vector(12, 49, 3), Vector(11, 49, 12), Vector(12, 50, 11), Vector(20, 11, 50),
          Vector(50, 21, 20), Vector(52, 40, 31), Vector(52, 41, 40), Vector(42, 41, 52), Vector(7, 8, 59),
          Vector(8, 9, 59), Vector(59, 9, 18), Vector(18, 17, 59), Vector(27, 26, 17, 18, 19, 28), Vector(30, 51, 31),
          Vector(51, 32, 31), Vector(51, 23, 32), Vector(51, 22, 23), Vector(21, 22, 51), Vector(21, 50, 22),
          Vector(23, 54, 32), Vector(50, 13, 22), Vector(23, 24, 54), Vector(13, 14, 15, 24, 23, 22),
          Vector(14, 55, 15), Vector(55, 16, 15), Vector(15, 16, 56), Vector(56, 24, 15), Vector(25, 54, 24),
          Vector(24, 56, 25), Vector(56, 26, 25), Vector(25, 34, 54), Vector(33, 32, 54), Vector(54, 34, 33),
          Vector(25, 26, 57), Vector(57, 34, 25), Vector(26, 27, 57), Vector(27, 36, 57), Vector(35, 34, 57),
          Vector(57, 36, 35), Vector(58, 35, 36), Vector(58, 44, 35), Vector(52, 31, 32), Vector(32, 33, 52),
          Vector(52, 33, 42), Vector(44, 43, 42, 33, 34, 35)
        )
      )
  }

  "An earlyUniform3" can "be divided in oriented polygons" in {
    val maybePolygons = earlyUniform3.tilingOrientedPolygons
    maybePolygons.get.size shouldBe 74
    maybePolygons shouldBe
      Option(
        List(
          Vector(14, 31, 57), Vector(31, 59, 58, 57), Vector(56, 55, 57, 58, 64, 63), Vector(63, 73, 62),
          Vector(59, 65, 58), Vector(63, 64, 74, 73), Vector(65, 69, 64, 58), Vector(64, 69, 74),
          Vector(14, 13, 28, 29, 30, 31), Vector(33, 34, 35, 36, 37, 23), Vector(40, 41, 42, 43, 39, 32),
          Vector(44, 51, 41, 40), Vector(37, 36, 48, 38), Vector(44, 38, 48, 49, 50, 51), Vector(30, 60, 59, 31),
          Vector(59, 60, 68, 67, 66, 65), Vector(65, 66, 75, 69), Vector(75, 78, 77, 76, 74, 69),
          Vector(87, 88, 97, 96, 95, 86), Vector(89, 103, 94), Vector(103, 102, 101, 100, 99, 94),
          Vector(76, 72, 73, 74), Vector(61, 70, 88, 87), Vector(72, 71, 70, 61, 62, 73), Vector(94, 99, 92, 93),
          Vector(32, 39, 25, 24), Vector(13, 12, 27, 28), Vector(12, 11, 24, 25, 26, 27), Vector(33, 81, 84, 34),
          Vector(92, 91, 84, 81, 82, 93), Vector(4, 16, 17, 18, 15, 3), Vector(16, 37, 38, 17), Vector(16, 23, 37),
          Vector(38, 44, 17), Vector(44, 40, 18, 17), Vector(40, 32, 18), Vector(18, 32, 24, 15), Vector(24, 11, 15),
          Vector(16, 4, 22, 23), Vector(11, 2, 3, 15), Vector(2, 1, 3), Vector(3, 1, 5, 4), Vector(4, 5, 22),
          Vector(11, 12, 10, 2), Vector(12, 13, 10), Vector(13, 14, 9, 10), Vector(14, 57, 55, 9), Vector(55, 8, 9),
          Vector(8, 7, 1, 2, 10, 9), Vector(7, 6, 5, 1), Vector(7, 45, 6), Vector(7, 8, 47, 45), Vector(8, 55, 56, 47),
          Vector(56, 52, 47), Vector(56, 63, 62, 52), Vector(5, 6, 19, 20, 21, 22), Vector(22, 21, 33, 23),
          Vector(21, 81, 33), Vector(19, 79, 20), Vector(20, 82, 81, 21), Vector(79, 83, 82, 20), Vector(83, 93, 82),
          Vector(83, 89, 94, 93), Vector(45, 46, 19, 6), Vector(19, 46, 80, 79), Vector(46, 54, 80),
          Vector(46, 45, 47, 52, 53, 54), Vector(62, 61, 53, 52), Vector(61, 87, 53), Vector(87, 86, 54, 53),
          Vector(86, 85, 80, 54), Vector(86, 95, 85), Vector(83, 79, 80, 85, 90, 89), Vector(95, 98, 90, 85)
        )
      )
  }

  it can "have listed all the unoriented polygons with an alt method" in {
    val polygons = earlyUniform3.tilingUnorientedPolygonsAlt(12)
    polygons.size shouldBe 74
    polygons shouldBe
      List(
        Vector(61, 62, 73, 72, 71, 70), Vector(17, 18, 40, 44), Vector(18, 32, 40), Vector(30, 31, 59, 60),
        Vector(92, 93, 94, 99), Vector(2, 10, 12, 11), Vector(15, 18, 32, 24), Vector(36, 37, 38, 48),
        Vector(85, 90, 98, 95), Vector(4, 5, 22), Vector(9, 14, 57, 55), Vector(79, 80, 85, 90, 89, 83),
        Vector(89, 94, 103), Vector(11, 15, 24), Vector(58, 59, 65), Vector(82, 83, 93), Vector(4, 16, 23, 22),
        Vector(52, 56, 63, 62), Vector(1, 3, 4, 5), Vector(1, 2, 3), Vector(94, 99, 100, 101, 102, 103),
        Vector(64, 69, 74), Vector(40, 41, 51, 44), Vector(6, 19, 46, 45), Vector(20, 21, 81, 82),
        Vector(12, 13, 28, 27), Vector(69, 74, 76, 77, 78, 75), Vector(53, 61, 87), Vector(33, 34, 84, 81),
        Vector(46, 54, 80), Vector(72, 73, 74, 76), Vector(45, 46, 54, 53, 52, 47), Vector(24, 25, 39, 32),
        Vector(11, 12, 27, 26, 25, 24), Vector(32, 39, 43, 42, 41, 40), Vector(16, 17, 38, 37), Vector(52, 53, 61, 62),
        Vector(21, 33, 81), Vector(10, 12, 13), Vector(47, 52, 56), Vector(55, 56, 63, 64, 58, 57), Vector(17, 38, 44),
        Vector(31, 57, 58, 59), Vector(20, 79, 83, 82), Vector(23, 33, 34, 35, 36, 37), Vector(9, 10, 13, 14),
        Vector(7, 8, 47, 45), Vector(1, 5, 6, 7), Vector(61, 70, 88, 87), Vector(85, 86, 95), Vector(2, 3, 15, 11),
        Vector(14, 31, 57), Vector(8, 47, 56, 55), Vector(63, 64, 74, 73), Vector(5, 6, 19, 20, 21, 22),
        Vector(62, 63, 73), Vector(19, 20, 79), Vector(58, 64, 69, 65), Vector(81, 82, 93, 92, 91, 84),
        Vector(65, 66, 75, 69), Vector(53, 54, 86, 87), Vector(38, 44, 51, 50, 49, 48), Vector(83, 89, 94, 93),
        Vector(8, 9, 55), Vector(86, 87, 88, 97, 96, 95), Vector(54, 80, 85, 86), Vector(3, 4, 16, 17, 18, 15),
        Vector(19, 46, 80, 79), Vector(59, 60, 68, 67, 66, 65), Vector(1, 2, 10, 9, 8, 7),
        Vector(13, 14, 31, 30, 29, 28), Vector(6, 7, 45), Vector(16, 23, 37), Vector(21, 22, 23, 33)
      )
  }

  "An uniformityIssue" can "be divided in oriented polygons" in {
    val maybePolygons = uniformityIssue.tilingOrientedPolygons
    maybePolygons.get.size shouldBe 128
    maybePolygons shouldBe
      Option(
        List(
          Vector(159, 171, 160), Vector(141, 165, 142),
          Vector(159, 158, 164, 143, 142, 165, 166, 167, 168, 169, 170, 171), Vector(55, 157, 54),
          Vector(133, 138, 134), Vector(56, 144, 55), Vector(55, 144, 164, 157), Vector(144, 143, 164),
          Vector(144, 56, 57, 137, 135, 134, 138, 139, 140, 141, 142, 143), Vector(149, 163, 150),
          Vector(164, 158, 157), Vector(149, 148, 156, 53, 54, 157, 158, 159, 160, 161, 162, 163), Vector(38, 136, 37),
          Vector(37, 58, 36), Vector(37, 136, 137, 58), Vector(137, 57, 58), Vector(49, 147, 48), Vector(50, 52, 49),
          Vector(52, 156, 147, 49), Vector(52, 53, 156), Vector(52, 50, 51, 42, 35, 36, 58, 57, 56, 55, 54, 53),
          Vector(113, 114, 122), Vector(112, 121, 111),
          Vector(113, 31, 30, 111, 121, 120, 119, 118, 117, 116, 115, 114), Vector(100, 123, 99), Vector(120, 129, 119),
          Vector(123, 124, 125, 126, 127, 128, 129, 120, 121, 112, 98, 99), Vector(136, 135, 137),
          Vector(116, 130, 115), Vector(136, 38, 39, 122, 114, 115, 130, 131, 132, 133, 134, 135),
          Vector(145, 155, 146), Vector(156, 148, 147),
          Vector(145, 47, 48, 147, 148, 149, 150, 151, 152, 153, 154, 155), Vector(178, 177, 213),
          Vector(153, 172, 154), Vector(178, 73, 72, 146, 155, 154, 172, 173, 174, 175, 176, 177),
          Vector(210, 214, 209), Vector(175, 220, 176),
          Vector(210, 211, 213, 177, 176, 220, 219, 218, 217, 216, 215, 214), Vector(223, 232, 103, 93),
          Vector(86, 27, 26), Vector(4, 25, 5), Vector(27, 28, 29, 30, 31, 32, 33, 25, 4, 16, 17, 26),
          Vector(31, 113, 32), Vector(32, 40, 33), Vector(32, 113, 122, 40), Vector(7, 34, 8), Vector(8, 41, 9),
          Vector(8, 34, 42, 41), Vector(34, 35, 42), Vector(122, 39, 40),
          Vector(34, 7, 6, 5, 25, 33, 40, 39, 38, 37, 36, 35), Vector(42, 51, 41), Vector(43, 44, 60),
          Vector(51, 50, 49, 48, 47, 46, 45, 44, 43, 10, 9, 41), Vector(46, 71, 45), Vector(47, 145, 46),
          Vector(46, 145, 146, 71), Vector(146, 72, 71), Vector(77, 76, 190),
          Vector(72, 73, 74, 75, 76, 77, 68, 69, 60, 44, 45, 71), Vector(29, 111, 30), Vector(93, 103, 94),
          Vector(28, 97, 29), Vector(29, 97, 112, 111), Vector(97, 98, 112),
          Vector(103, 102, 101, 100, 99, 98, 97, 28, 27, 86, 95, 94), Vector(73, 178, 74), Vector(74, 212, 75),
          Vector(74, 178, 213, 212), Vector(183, 206, 184), Vector(213, 211, 212),
          Vector(183, 182, 190, 76, 75, 212, 211, 210, 209, 208, 207, 206), Vector(222, 231, 221), Vector(92, 223, 93),
          Vector(231, 230, 229, 228, 227, 226, 225, 224, 223, 92, 91, 221),
          Vector(2, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), Vector(2, 3, 1), Vector(1, 5, 6), Vector(1, 3, 4, 5),
          Vector(13, 61, 14), Vector(14, 24, 15), Vector(61, 70, 24, 14), Vector(70, 23, 24), Vector(3, 16, 4),
          Vector(23, 22, 21, 20, 19, 18, 17, 16, 3, 2, 15, 24), Vector(18, 26, 17), Vector(19, 85, 18),
          Vector(18, 85, 86, 26), Vector(11, 59, 12), Vector(10, 43, 11), Vector(11, 43, 60, 59), Vector(61, 62, 70),
          Vector(60, 69, 59), Vector(61, 13, 12, 59, 69, 68, 67, 66, 65, 64, 63, 62), Vector(67, 181, 66),
          Vector(68, 77, 67), Vector(77, 190, 181, 67), Vector(64, 78, 63), Vector(21, 87, 20), Vector(22, 84, 21),
          Vector(21, 84, 96, 87), Vector(84, 83, 96), Vector(65, 179, 64), Vector(64, 179, 180, 78),
          Vector(180, 79, 78), Vector(84, 22, 23, 70, 62, 63, 78, 79, 80, 81, 82, 83), Vector(85, 95, 86),
          Vector(96, 88, 87), Vector(85, 19, 20, 87, 88, 89, 90, 91, 92, 93, 94, 95), Vector(190, 182, 181),
          Vector(179, 189, 180), Vector(182, 183, 184, 185, 186, 187, 188, 189, 179, 65, 66, 181), Vector(80, 191, 81),
          Vector(81, 110, 82), Vector(191, 198, 110, 81), Vector(187, 197, 188), Vector(191, 192, 198),
          Vector(197, 196, 195, 194, 193, 192, 191, 80, 79, 180, 189, 188), Vector(194, 205, 193),
          Vector(198, 109, 110), Vector(205, 204, 203, 202, 201, 200, 199, 108, 109, 198, 192, 193),
          Vector(89, 104, 90), Vector(90, 221, 91), Vector(90, 104, 222, 221), Vector(199, 107, 108),
          Vector(104, 105, 222), Vector(107, 106, 105, 104, 89, 88, 96, 83, 82, 110, 109, 108)
        )
      )
  }

  it can "have listed all the unoriented polygons with an alt method" in {
    val polygons = uniformityIssue.tilingUnorientedPolygonsAlt(12)
    polygons.size shouldBe 128
  }

  "The minimal tiling with a complex inner perimeter" can "be divided in oriented polygons" in {
    minimalComplexInnerPerimeter.tilingOrientedPolygons shouldBe
      Option(
        List(
          Vector(2, 4, 7, 6, 3, 1), Vector(4, 8, 7), Vector(7, 10, 6), Vector(8, 11, 15, 14, 10, 7), Vector(15, 17, 14),
          Vector(14, 13, 10), Vector(17, 19, 18, 16, 13, 14), Vector(16, 12, 13), Vector(6, 5, 3),
          Vector(12, 9, 5, 6, 10, 13)
        )
      )
  }
}
