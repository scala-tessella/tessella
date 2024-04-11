package io.github.scala_tessella.tessella
package creation

import TilingUniformity.uniformity
import conversion.SVG.{MarkStyle, toSVG}

trait SymmetricHelper extends Helper with Accuracy:

  extension (tiling: Tiling)

    def filterUniformity(value: Int)(allowGrowth: Int): Boolean =
      val (gonality, uniformity): (Int, Int) =
        (tiling.gonality, tiling.uniformity)
      uniformity <= value &&
        ((tiling.countPolygons < allowGrowth && gonality == uniformity)
          || (gonality == value /*&& tiling.compactness > 0.5*/ && uniformity == value))

  extension (tilings: List[Tiling])

    def saveSymmetries(folder: String, prefix: String): Unit =
      tilings.indices.foreach(index =>
        saveFileSVG(
          tilings(index).toSVG (fillPolygons = true, markStyle = MarkStyle.UNIFORMITY),
          s"symmetric/$folder/${prefix}_${index}_${tilings(index).compactness.rounded()}"
        )
      )
