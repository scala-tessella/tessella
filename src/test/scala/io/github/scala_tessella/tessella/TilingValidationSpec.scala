package io.github.scala_tessella.tessella

import Outliers.*
import Topology.{--, Edge}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingValidationSpec extends AnyFlatSpec with should.Matchers {

  val tiling: Tiling = Tiling.fromPolygon(3)

  "A Tiling" must "be compacted, all nodes labelled from 1 to next" in {
    val interruptedEdges: List[Edge] =
      List(1--2, 1--4, 2--4)
    Tiling.maybe(interruptedEdges) shouldEqual
      Left(
        """Tiling not compacted, nodes should go uninterrupted from 1 to 3:
          | found node 4.
          |See DOT:
          |graph{
          |1 -- 2
          |1 -- 4
          |2 -- 4
          |4 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it can "NOT have a node originating just one edge" in {
    val oneDegree = tiling.edges :+ 3--4
    Tiling.maybe(oneDegree) shouldEqual
      Left(
        """Tiling must have each node connected to min 2 and max 6 other nodes, these nodes are not compliant: (4 [degree 1]).
          |See DOT:
          |graph{
          |1 -- 2
          |2 -- 3
          |1 -- 3
          |3 -- 4 [color=red]
          |4 [color=red fontcolor=red]
          |}""".stripMargin
    )
  }

  it can "NOT have a node originating more than six edges" in {
    val sevenDegree = Tiling.fromPolygon(7).edges ++ (1 to 7).map(_--8)
    Tiling.maybe(sevenDegree) shouldEqual
      Left(
        """Tiling must have each node connected to min 2 and max 6 other nodes, these nodes are not compliant: (8 [degree 7]).
          |See DOT:
          |graph{
          |1 -- 2
          |2 -- 3
          |3 -- 4
          |4 -- 5
          |5 -- 6
          |6 -- 7
          |1 -- 7
          |1 -- 8 [color=red]
          |2 -- 8 [color=red]
          |3 -- 8 [color=red]
          |4 -- 8 [color=red]
          |5 -- 8 [color=red]
          |6 -- 8 [color=red]
          |7 -- 8 [color=red]
          |8 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it must "be connected" in {
    minimalDisconnected.toMaybeTiling shouldEqual
      Left(
        """Tiling disconnected, these sets of nodes are not connected to each other: (4, 5, 6), (1, 2, 3).
          |See DOT:
          |graph{
          |1 -- 2
          |2 -- 3
          |1 -- 3
          |4 -- 5 [color=red]
          |5 -- 6 [color=red]
          |4 -- 6 [color=red]
          |4 [color=red fontcolor=red]
          |5 [color=red fontcolor=red]
          |6 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it must "have a valid perimeter" in {
    minimalThinlyConnected.toMaybeTiling shouldEqual
      Left(
        """Perimeter must have each node connected to exactly 2 other nodes, these nodes are not compliant: (3 [degree 4]).
          |See DOT:
          |graph{
          |1 -- 2
          |2 -- 3 [color=red]
          |1 -- 3 [color=red]
          |3 -- 4 [color=red]
          |4 -- 5
          |3 -- 5 [color=red]
          |3 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it can "NOT have an invalid (4³,5) more than full vertex on a non-perimeter node" in {
    val withInvalidVertex =
      (squareReticulate(2).graphEdges ++ List(9--10, 8--10)).filterNot(_ == 8--9)
    Tiling.maybe(withInvalidVertex) shouldEqual
      Left(
        """Tiling must have all internal nodes as valid FullVertex:
          | found invalid node 5.
          |See DOT:
          |graph{
          |1 -- 2
          |4 -- 5
          |7 -- 8
          |2 -- 3
          |5 -- 6
          |1 -- 4
          |4 -- 7
          |2 -- 5
          |5 -- 8
          |3 -- 6
          |6 -- 9
          |9 -- 10
          |8 -- 10
          |5 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it can "NOT have an invalid (4³,5) more than full vertex on a perimeter node" in {
    val withInvalidVertex =
      (squareReticulate(2).graphEdges ++ List(9--10, 10--11, 11--5)).filterNot(_ == 8--9)
    Tiling.maybe(withInvalidVertex) shouldEqual
      Left(
        """Tiling must have all perimeter nodes as valid perimeter vertex:
          | found invalid node 5.
          |See DOT:
          |graph{
          |1 -- 2
          |4 -- 5
          |7 -- 8
          |2 -- 3
          |5 -- 6
          |1 -- 4
          |4 -- 7
          |2 -- 5
          |5 -- 8
          |3 -- 6
          |6 -- 9
          |9 -- 10
          |10 -- 11
          |5 -- 11
          |5 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  it can "NOT have an inside gap" in {
    val withInvalidVertex =
      Tiling.squareNet(3, 4).toOption.get.graphEdges.filterNot(_ == 10--11)
    Tiling.maybe(withInvalidVertex) shouldEqual
      Left(
        """Tiling must have all internal nodes as valid FullVertex:
          | found invalid nodes (6, 10, 14, 7, 15, 11).
          |See DOT:
          |graph{
          |1 -- 2
          |5 -- 6
          |9 -- 10
          |13 -- 14
          |17 -- 18
          |2 -- 3
          |6 -- 7
          |14 -- 15
          |18 -- 19
          |3 -- 4
          |7 -- 8
          |11 -- 12
          |15 -- 16
          |19 -- 20
          |1 -- 5
          |5 -- 9
          |9 -- 13
          |13 -- 17
          |2 -- 6
          |6 -- 10
          |10 -- 14
          |14 -- 18
          |3 -- 7
          |7 -- 11
          |11 -- 15
          |15 -- 19
          |4 -- 8
          |8 -- 12
          |12 -- 16
          |16 -- 20
          |6 [color=red fontcolor=red]
          |10 [color=red fontcolor=red]
          |14 [color=red fontcolor=red]
          |7 [color=red fontcolor=red]
          |15 [color=red fontcolor=red]
          |11 [color=red fontcolor=red]
          |}""".stripMargin
      )
  }

  val smallestTilingWithInvalidSameVertices: List[Edge] =
    11--12 :: commonEdges

  val smallestTilingWithInvalidCrossingSides: List[Edge] =
    commonEdges ++ List(11--13, 12--13)

  val baseEdges: List[Edge] =
    List(1--2, 2--3, 3--4, 4--1, 4--5, 5--6, 6--3, 5--7, 7--8, 8--6, 8--9, 9--10, 10--6, 3--11, 11--12, 12--13, 13--14, 14--2, 13--15, 15--16, 16--12, 12--17, 17--18, 18--16, 17--19, 19--20, 20--18)

  val sharingAreaAndSides: List[Edge] =
    baseEdges ++ List(17--21, 21--22, 22--19)

  val sharingSides: List[Edge] =
    baseEdges ++ List(19--21, 21--22, 22--20, 19--23, 23--24, 24--21)

  it can "NOT have perimeter nodes with the same cartesian coords" in {
    Tiling.maybe(smallestTilingWithInvalidSameVertices) shouldEqual
      Left(
        """Tiling must have all perimeter nodes at different cartesian coords:
          | found invalid couple (12,10).
          |See SVG:
          |<svg viewBox="-93.30127 -25.0 193.30127 186.60254" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling perimeter</title>
          |    <desc>Invalid touching vertices</desc>
          |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 -43.30127,25.0 -68.30127,68.30127 -43.30127,111.60254 0.0,136.60254 25.0,93.30127 -18.30127,68.30127 25.0,43.30127 25.0,93.30127 75.0,93.30127 75.0,43.30127 50.0,0.0"/>
          |    <g style="fill:red;stroke:orangered">
          |      <title>Perimeter intersections</title>
          |      <desc>Perimeter edges intersecting</desc>
          |      <circle cx="25.0" cy="93.30127" r="5.0"/>
          |    </g>
          |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |      <title>Perimeter node labels</title>
          |      <desc>Each perimeter node showing its value</desc>
          |      <text x="-68.30127" y="68.30127">5</text>
          |      <text x="25.0" y="93.30127">10</text>
          |      <text x="0.0" y="0.0">1</text>
          |      <text x="50.0" y="0.0">6</text>
          |      <text x="75.0" y="93.30127">9</text>
          |      <text x="-43.30127" y="25.0">2</text>
          |      <text x="25.0" y="93.30127">12</text>
          |      <text x="75.0" y="43.30127">7</text>
          |      <text x="-18.30127" y="68.30127">3</text>
          |      <text x="0.0" y="136.60254">11</text>
          |      <text x="-43.30127" y="111.60254">8</text>
          |      <text x="25.0" y="43.30127">4</text>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have perimeter with crossing sides" in {
    Tiling.maybe(smallestTilingWithInvalidCrossingSides) shouldEqual
      Left(
        """Tiling must not have intersecting perimeter edges:
          | found invalid couples ((4--10, 3--12), (9--10, 12--13)).
          |See SVG:
          |<svg viewBox="-93.30127 -25.0 193.30127 198.759782" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling perimeter</title>
          |    <desc>Invalid intersecting edges</desc>
          |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 -43.30127,25.0 -68.30127,68.30127 -43.30127,111.60254 -9.84474,148.759782 35.832533,128.422949 30.60611,78.696855 -18.30127,68.30127 25.0,43.30127 25.0,93.30127 75.0,93.30127 75.0,43.30127 50.0,0.0"/>
          |    <g style="fill:red;stroke:orangered">
          |      <title>Perimeter intersections</title>
          |      <desc>Perimeter edges intersecting</desc>
          |      <circle cx="25.0" cy="77.505239" r="5.0"/>
          |      <circle cx="32.141096" cy="93.30127" r="5.0"/>
          |    </g>
          |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |      <title>Perimeter node labels</title>
          |      <desc>Each perimeter node showing its value</desc>
          |      <text x="-68.30127" y="68.30127">5</text>
          |      <text x="25.0" y="93.30127">10</text>
          |      <text x="0.0" y="0.0">1</text>
          |      <text x="50.0" y="0.0">6</text>
          |      <text x="75.0" y="93.30127">9</text>
          |      <text x="35.832533" y="128.422949">13</text>
          |      <text x="-43.30127" y="25.0">2</text>
          |      <text x="30.60611" y="78.696855">12</text>
          |      <text x="75.0" y="43.30127">7</text>
          |      <text x="-18.30127" y="68.30127">3</text>
          |      <text x="-9.84474" y="148.759782">11</text>
          |      <text x="-43.30127" y="111.60254">8</text>
          |      <text x="25.0" y="43.30127">4</text>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have perimeter with shared area and overlapping sides" in {
    Tiling.maybe(sharingAreaAndSides) shouldEqual
      Left(
        """Tiling must not have intersecting perimeter edges:
          | found invalid couples ((6--10, 21--22), (6--10, 17--21), (9--10, 17--21), (8--9, 19--22)).
          |See SVG:
          |<svg viewBox="-50.0 -25.0 225.0 236.60254" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling perimeter</title>
          |    <desc>Invalid intersecting edges</desc>
          |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 0.0,50.0 -25.0,93.30127 0.0,136.60254 0.0,186.60254 50.0,186.60254 100.0,186.60254 150.0,186.60254 150.0,136.60254 150.0,86.60254 100.0,86.60254 100.0,136.60254 50.0,136.60254 75.0,93.30127 50.0,50.0 100.0,50.0 100.0,100.0 150.0,100.0 150.0,50.0 150.0,0.0 100.0,0.0 50.0,0.0"/>
          |    <g style="fill:red;stroke:orangered">
          |      <title>Perimeter intersections</title>
          |      <desc>Perimeter edges intersecting</desc>
          |      <circle cx="100.0" cy="86.60254" r="5.0"/>
          |      <circle cx="100.0" cy="100.0" r="5.0"/>
          |    </g>
          |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |      <title>Perimeter node labels</title>
          |      <desc>Each perimeter node showing its value</desc>
          |      <text x="100.0" y="0.0">5</text>
          |      <text x="100.0" y="100.0">10</text>
          |      <text x="-25.0" y="93.30127">14</text>
          |      <text x="150.0" y="186.60254">20</text>
          |      <text x="0.0" y="0.0">1</text>
          |      <text x="100.0" y="50.0">6</text>
          |      <text x="100.0" y="86.60254">21</text>
          |      <text x="150.0" y="100.0">9</text>
          |      <text x="0.0" y="136.60254">13</text>
          |      <text x="0.0" y="50.0">2</text>
          |      <text x="100.0" y="136.60254">17</text>
          |      <text x="150.0" y="86.60254">22</text>
          |      <text x="50.0" y="136.60254">12</text>
          |      <text x="150.0" y="0.0">7</text>
          |      <text x="50.0" y="50.0">3</text>
          |      <text x="100.0" y="186.60254">18</text>
          |      <text x="50.0" y="186.60254">16</text>
          |      <text x="75.0" y="93.30127">11</text>
          |      <text x="150.0" y="50.0">8</text>
          |      <text x="150.0" y="136.60254">19</text>
          |      <text x="50.0" y="0.0">4</text>
          |      <text x="0.0" y="186.60254">15</text>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have perimeter with overlapping sides" in {
    Tiling.maybe(sharingSides) shouldEqual
      Left(
        """Tiling must not have intersecting perimeter edges:
          | found invalid couples ((19--23, 9--10), (23--24, 8--9)).
          |See SVG:
          |<svg viewBox="-25.0 -50.0 236.60254 275.0" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling perimeter</title>
          |    <desc>Invalid intersecting edges</desc>
          |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 0.0,50.0 0.0,100.0 0.0,150.0 50.0,150.0 100.0,150.0 100.0,100.0 50.0,100.0 50.0,50.0 93.30127,75.0 136.60254,50.0 136.60254,100.0 136.60254,150.0 86.60254,150.0 86.60254,200.0 136.60254,200.0 186.60254,200.0 186.60254,150.0 186.60254,100.0 186.60254,50.0 186.60254,0.0 136.60254,0.0 93.30127,-25.0 50.0,0.0"/>
          |    <g style="fill:red;stroke:orangered">
          |      <title>Perimeter intersections</title>
          |      <desc>Perimeter edges intersecting</desc>
          |      <circle cx="100.0" cy="150.0" r="5.0"/>
          |      <circle cx="86.60254" cy="150.0" r="5.0"/>
          |    </g>
          |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |      <title>Perimeter node labels</title>
          |      <desc>Each perimeter node showing its value</desc>
          |      <text x="0.0" y="100.0">5</text>
          |      <text x="100.0" y="100.0">10</text>
          |      <text x="86.60254" y="200.0">24</text>
          |      <text x="93.30127" y="-25.0">14</text>
          |      <text x="186.60254" y="150.0">20</text>
          |      <text x="0.0" y="0.0">1</text>
          |      <text x="50.0" y="100.0">6</text>
          |      <text x="136.60254" y="200.0">21</text>
          |      <text x="100.0" y="150.0">9</text>
          |      <text x="136.60254" y="0.0">13</text>
          |      <text x="50.0" y="0.0">2</text>
          |      <text x="136.60254" y="100.0">17</text>
          |      <text x="186.60254" y="200.0">22</text>
          |      <text x="136.60254" y="50.0">12</text>
          |      <text x="0.0" y="150.0">7</text>
          |      <text x="50.0" y="50.0">3</text>
          |      <text x="186.60254" y="100.0">18</text>
          |      <text x="186.60254" y="50.0">16</text>
          |      <text x="93.30127" y="75.0">11</text>
          |      <text x="86.60254" y="150.0">23</text>
          |      <text x="50.0" y="150.0">8</text>
          |      <text x="136.60254" y="150.0">19</text>
          |      <text x="0.0" y="50.0">4</text>
          |      <text x="186.60254" y="0.0">15</text>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  "An outlier tiling" must "be valid" in {
    Tiling.maybe(withFalseInternalPerimeter).isRight shouldBe
      true
  }
  
  "Another one" must "be valid" in {
    Tiling.maybe(withFalseInternalPerimeter2).isRight shouldBe
      true
  }

  "A third one" must "be valid" in {
    Tiling.maybe(withFalseInternalPerimeter3).isRight shouldBe
      true
  }

  "A fourth one" can "be be searched for unoriented polygons" in {
    Graph(problematicEdges).tilingUnorientedPolygons shouldBe
      Option(
        List(
          Vector(5, 31, 34, 6), Vector(31, 30, 5), Vector(30, 4, 5), Vector(34, 40, 6), Vector(40, 7, 6),
          Vector(4, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5), Vector(11, 43, 10), Vector(43, 37, 11),
          Vector(12, 32, 37, 11), Vector(12, 13, 22), Vector(12, 32, 22), Vector(2, 3, 1),
          Vector(3, 14, 15, 16, 17, 18, 19, 20, 21, 22, 13, 1), Vector(15, 23, 14), Vector(15, 33, 23),
          Vector(33, 38, 16, 15), Vector(16, 17, 44), Vector(16, 38, 44), Vector(32, 36, 21, 22), Vector(21, 20, 41),
          Vector(21, 36, 41), Vector(23, 24, 35, 33), Vector(30, 29, 39, 31), Vector(39, 45, 29), Vector(45, 28, 29),
          Vector(24, 42, 35), Vector(24, 25, 42), Vector(28, 27, 26, 25, 24, 23, 14, 3, 2, 4, 30, 29),
          Vector(36, 48, 47, 46, 37, 32), Vector(35, 51, 50, 49, 38, 33), Vector(39, 52, 53, 54, 34, 31)
        )
      )
  }

  it must "be valid" in {
    Tiling.maybe(problematicEdges).isRight shouldBe
      true
  }

}
