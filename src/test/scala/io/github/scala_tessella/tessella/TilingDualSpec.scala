package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import Topology.Node
import TopologyDual.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingDualSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A triangle" can "be converted" in {
    triangle.toTilingDual.toString shouldBe
      "TilingDual((1 p3))"
  }

  "An hexagon made of triangles" can "be converted" in {
    hexagonTriangles.toTilingDual.toString shouldBe
      "TilingDual((6 p3, 4 p3, 3 p3, 1 p3, 5 p3, 2 p3), 1--2, 1--5, 2--3, 3--4, 4--6, 5--6)"
  }

}
