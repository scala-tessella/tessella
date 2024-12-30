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
      "TilingDual(Vector(1, 2, 3) 1--4, 2--4, 3--4)"
    triangle.toTilingDual.polygonBoundary shouldBe
      Vector(3, 3, 3)
  }

  "An hexagon made of triangles" can "be converted" in {
    hexagonTriangles.toTilingDual.toString shouldBe
      "TilingDual(Vector(1, 2, 3, 4, 5, 6) 1--9, 2--10, 3--12, 4--11, 5--7, 6--8, 7--8, 7--11, 8--9, 9--10, 10--12, 11--12)"
    hexagonTriangles.toTilingDual.polygonBoundary shouldBe
      Vector(3, 3, 3, 3, 3, 3)
  }

}
