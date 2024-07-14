package io.github.scala_tessella.tessella

import RegularPolygon.Polygon
import TilingGrowth.OtherNodeStrategy.BEFORE_PERIMETER
import Topology.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingGrowthEdgeSpec extends AnyFlatSpec with Helper with should.Matchers {

//  "A tiling" can "have some polygons added to an edge" in {
//    square.maybeGrowEdge(1--2, Polygon(4), BEFORE_PERIMETER).unsafe.allLabels shouldBe
//      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-25.0 -75.0 100.0 150.0" xmlns="http://www.w3.org/2000/svg">
//        |  <g>
//        |    <title>Tiling</title>
//        |    <desc>Finite tessellation of regular polygons</desc>
//        |    <g style="stroke:black">
//        |      <title>Edges</title>
//        |      <desc>Sides of the regular polygons</desc>
//        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
//        |      <line x1="50.0" y1="0.0" x2="50.0" y2="50.0"/>
//        |      <line x1="50.0" y1="50.0" x2="0.0" y2="50.0"/>
//        |      <line x1="0.0" y1="0.0" x2="0.0" y2="50.0"/>
//        |      <line x1="50.0" y1="0.0" x2="50.0" y2="-50.0"/>
//        |      <line x1="50.0" y1="-50.0" x2="0.0" y2="-50.0"/>
//        |      <line x1="0.0" y1="0.0" x2="0.0" y2="-50.0"/>
//        |    </g>
//        |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 0.0,50.0 50.0,50.0 50.0,0.0 50.0,-50.0 0.0,-50.0"/>
//        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
//        |      <title>Node labels</title>
//        |      <desc>Each node showing its value</desc>
//        |      <text x="50.0" y="-50.0">5</text>
//        |      <text x="0.0" y="0.0">1</text>
//        |      <text x="0.0" y="-50.0">6</text>
//        |      <text x="50.0" y="0.0">2</text>
//        |      <text x="50.0" y="50.0">3</text>
//        |      <text x="0.0" y="50.0">4</text>
//        |    </g>
//        |  </g>
//        |  <metadata>
//        |    <rdf:RDF>
//        |      <cc:Work>
//        |        <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>
//        |        <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>
//        |      </cc:Work>
//        |    </rdf:RDF>
//        |  </metadata>
//        |</svg>""".stripMargin
//  }

  "A tiling" can "NOT have a polygon added to a non existing edge" in {
    Tiling.pattern_4444(2, 2).unsafe.maybeGrowEdge(1--5, Polygon(4), BEFORE_PERIMETER).left.getOrElse("").take(83) shouldBe
      """Tiling can add polygons only to perimeter edges:
        | found unknown edge 1--5.
        |See SVG:""".stripMargin
  }

  it can "NOT have a polygon added to a non perimeter edge" in {
    Tiling.pattern_4444(2, 2).unsafe.maybeGrowEdge(2--5, Polygon(4), BEFORE_PERIMETER).left.getOrElse("").take(81) shouldBe
      """Tiling can add polygons only to perimeter edges:
        | found inner edge 2--5.
        |See SVG:""".stripMargin
  }

  val strange: Tiling =
    Tiling
      .maybe(
        List(
          2--3, 3--4, 1--4, 1--2, 2--5, 5--6, 1--6, 1--7, 7--8, 4--8, 3--9, 9--10, 4--10, 2--11, 11--12, 3--12,
          7--13, 6--13, 5--14, 11--14, 12--15, 9--15, 10--16, 8--16,
          5--17, 17--18, 14--18, 12--19, 19--20, 15--20, 10--21, 21--22, 16--22, 7--23, 23--24, 13--24,
          6--25, 17--25, 9--26, 21--26, 13--27, 25--27, 15--28, 26--28
        )
      )
      .unsafe

  "A strange tiling" can "be validly grown" in {
    strange.maybeGrowEdge(7--23, Polygon(12), BEFORE_PERIMETER).isRight shouldBe
      false
  }
}
