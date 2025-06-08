package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.conversion.SVG.toSVG

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class OutliersToSVGFileSpec extends AnyFlatSpec with Helper with should.Matchers:

  val allOutliers: List[Tiling] =
    Outliers.all

  "All outliers" must "be valid" in {
    allOutliers.forall(tiling =>
      try {
        val edges = tiling.graphEdges
        println(edges.size)
        true
      } catch {
        case _: Throwable => false
      }
    ) shouldBe true
  }

//  they must "be saved as .SVG files to a dir" in {
//    Outliers.all.indices.foreach(index =>
//      println(index)
//      try {
//        val tiling: Tiling = Outliers.all(index)
//        saveFileSVG(tiling.toSVG(), s"test/$index")
//      } catch {
//        case e: Throwable => println(s"Error for $index: $e")
//      }
//    )
//  }
