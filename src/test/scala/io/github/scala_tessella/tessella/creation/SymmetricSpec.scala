package io.github.scala_tessella.tessella
package creation

import Outliers.*
import Symmetric.*
import TilingSymmetry.countSymmetries
import TilingUniformity.uniformity

import io.github.scala_tessella.ring_seq.SymmetryOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SymmetricSpec extends AnyFlatSpec with SymmetryOps with SymmetricHelper with should.Matchers{

  "Single polygons to be added" can "be listed" in {
    additionalPolygons shouldBe
      List(3, 4, 6, 8, 12)
  }

  they can "be listed without octagon" in {
    additionalPolygonsWithoutOctagon shouldBe
      List(3, 4, 6, 12)
  }

  "A triangle" can "have sets of perimeter nodes at minimal angle where to apply growth" in {
    triangle.rotationalPerimeterNodeSets shouldBe
      (
        List(List(1, 3, 2)),
        additionalPolygonsWithoutOctagon
      )
  }

  "A square" can "have sets of perimeter nodes at minimal angle where to apply growth" in {
    square.rotationalPerimeterNodeSets shouldBe
      (
        List(List(1, 3)),
        additionalPolygons
      )
  }

  "A 3464 hexoid" can "have sets of perimeter nodes at minimal angle where to apply growth" in {
    triSqrHexHexoid.rotationalPerimeterNodeSets shouldBe
      (
        List(List(19, 35, 27), List(19, 31)),
        List(3, 4, 12)
      )
  }

  "A two triangles tiling" can "have sets of perimeter nodes at minimal angle where to apply growth" in {
    minimalDifferentFromItsPeri.toMaybeTiling.unsafe.rotationalPerimeterNodeSets shouldBe
      (
        List(List(1, 3)),
        additionalPolygonsWithoutOctagon
      )
  }

  "A triangle" can "be expanded by a single step to 4 variants" in {
    val tilings: List[Tiling] =
      triangle.expansionStep
//    tilings.indices.foreach(index =>
//      saveFile(
//        tilings(index).toSVG(fillPolygons = true, markStyle = MarkStyle.UNIFORMITY),
//        s"symmetric/t$index"
//      )
//    )
    tilings.size shouldBe
      4
  }

  "A square" can "be expanded by a single step to 5 variants" in {
    val tilings: List[Tiling] =
      square.expansionStep
//    tilings.indices.foreach(index =>
//      saveFile(
//        tilings(index).toSVG(fillPolygons = true, markStyle = MarkStyle.UNIFORMITY),
//        s"symmetric/s$index"
//      )
//    )
    tilings.size shouldBe
      5
  }

    "An hexagon" can "be expanded by a single step to 8 variants" in {
    val tilings: List[Tiling] =
      hexagon.expansionStep
//    tilings.indices.foreach(index =>
//      saveFile(
//        tilings(index).toSVG(fillPolygons = true, markStyle = MarkStyle.UNIFORMITY),
//        s"symmetric/h$index"
//      )
//    )
    tilings.size shouldBe
      8
  }

  "A triangle" can "be expanded into a regular tessellation" in {
    val tilings: List[Tiling] =
      triangle.expansion(24, _.hedrality == 1)
//    tilings.saveSymmetries("regular", "t")
    tilings.size shouldBe
      1
  }

  "A square" can "be expanded into a regular tessellation" in {
    val tilings: List[Tiling] =
      square.expansion(24, t => t.hedrality == 1)
//    tilings.saveSymmetries("regular", "s")
    tilings.size shouldBe
      1
  }

  "An hexagon" can "be expanded into a regular tessellation" in {
    val tilings: List[Tiling] =
      hexagon.expansion(6, t => t.hedrality == 1)
        .filter(_.countSymmetries == 6)
//    tilings.saveSymmetries("regular", "h")
    tilings.size shouldBe
      1
  }

  "A triangle" can "be expanded into a semiregular tessellations" in {
    val tilings: List[Tiling] =
      triangle.expansion(10, t => t.uniformity == 1 && (t.countPolygons < 5 || t.hedrality > 1))
//    tilings.saveSymmetries("semiregular", "t")
    tilings.size shouldBe
      4
  }

  "A square" can "be expanded into semiregular tessellations" in {
    val tilings: List[Tiling] =
      square.expansion(12, t => t.uniformity == 1 && t.hedrality > 1)
//    tilings.saveSymmetries("semiregular", "s")
    tilings.size shouldBe
      5
  }

}
