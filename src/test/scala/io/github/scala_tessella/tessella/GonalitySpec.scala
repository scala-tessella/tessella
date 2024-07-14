package io.github.scala_tessella.tessella

import Outliers.*
import RegularPolygon.Vertex
import Topology.Node
import utility.Utils.mapKeys

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class GonalitySpec extends AnyFlatSpec with Helper with should.Matchers {

  "A 3 uniform tiling" can "return a map of full vertices and the nodes where they form" in {
    p2x333333_33336.groupGonals shouldBe
      Map(
        Vector(3, 3, 3, 3, 6) -> List(24, 37, 14, 33, 13, 17, 34, 22, 27, 35, 26, 23, 15),
        Vector(3, 3, 3, 3, 3, 3) -> List(56, 25, 52, 57, 60, 53, 32, 59, 12, 54, 49, 50, 16, 55, 58, 36, 51)
      ).mapKeys(_.map(Node(_)))
  }

  "Another tiling" can "return a map of full vertices and the nodes where they form" in {
    gonExperiment.groupGonals shouldBe
      Map(
        Vector(4, 4, 6) -> List(9, 8),
        Vector(4, 4, 4, 4) -> List(1, 2, 3, 4),
        Vector(4, 4, 8) -> List(14, 15),
        Vector(3, 3, 4, 4) -> List(5)
      ).mapKeys(_.map(Node(_)))
  }

}
