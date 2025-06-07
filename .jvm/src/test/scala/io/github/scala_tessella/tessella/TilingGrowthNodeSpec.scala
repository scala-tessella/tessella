package io.github.scala_tessella.tessella

import Outliers.edges12Nodes8
import RegularPolygon.{Polygon, Vertex}
import TilingGrowth.*
import TilingGrowth.OtherNodeStrategy.*
import Topology.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingGrowthNodeSpec extends AnyFlatSpec with Helper with should.Matchers {

  val vertex3344: Vertex =
    Vertex(3, 3, 4, 4)

  "A tiling" can "return a new tiling with polygons added to a perimeter node" in {
    val t: Tiling =
      edges12Nodes8.maybeGrowNode(Node(1), vertex3344, FIXED(Node(3))).unsafe
    t.allLabels shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-75.0 -75.0 268.30127 168.30127" xmlns="http://www.w3.org/2000/svg">
        |  <g>
        |    <title>Tiling</title>
        |    <desc>Finite tessellation of regular polygons</desc>
        |    <g style="stroke:black">
        |      <title>Edges</title>
        |      <desc>Sides of the regular polygons</desc>
        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
        |      <line x1="0.0" y1="0.0" x2="25.0" y2="43.30127"/>
        |      <line x1="50.0" y1="0.0" x2="25.0" y2="43.30127"/>
        |      <line x1="50.0" y1="0.0" x2="93.30127" y2="25.0"/>
        |      <line x1="93.30127" y1="25.0" x2="68.30127" y2="68.30127"/>
        |      <line x1="25.0" y1="43.30127" x2="68.30127" y2="68.30127"/>
        |      <line x1="93.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
        |      <line x1="68.30127" y1="68.30127" x2="118.30127" y2="68.30127"/>
        |      <line x1="93.30127" y1="25.0" x2="143.30127" y2="25.0"/>
        |      <line x1="143.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
        |      <line x1="143.30127" y1="25.0" x2="168.30127" y2="68.30127"/>
        |      <line x1="118.30127" y1="68.30127" x2="168.30127" y2="68.30127"/>
        |      <line x1="25.0" y1="43.30127" x2="-25.0" y2="43.30127"/>
        |      <line x1="0.0" y1="0.0" x2="-25.0" y2="43.30127"/>
        |      <line x1="50.0" y1="0.0" x2="50.0" y2="-50.0"/>
        |      <line x1="50.0" y1="-50.0" x2="0.0" y2="-50.0"/>
        |      <line x1="0.0" y1="0.0" x2="0.0" y2="-50.0"/>
        |      <line x1="0.0" y1="-50.0" x2="-50.0" y2="-50.0"/>
        |      <line x1="-50.0" y1="-50.0" x2="-50.0" y2="0.0"/>
        |      <line x1="0.0" y1="0.0" x2="-50.0" y2="0.0"/>
        |      <line x1="-25.0" y1="43.30127" x2="-50.0" y2="0.0"/>
        |    </g>
        |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="50.0,0.0 93.30127,25.0 143.30127,25.0 168.30127,68.30127 118.30127,68.30127 68.30127,68.30127 25.0,43.30127 -25.0,43.30127 -50.0,0.0 -50.0,-50.0 0.0,-50.0 50.0,-50.0"/>
        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
        |      <title>Node labels</title>
        |      <desc>Each node showing its value</desc>
        |      <text x="68.30127" y="68.30127">5</text>
        |      <text x="50.0" y="-50.0">10</text>
        |      <text x="0.0" y="0.0">1</text>
        |      <text x="143.30127" y="25.0">6</text>
        |      <text x="-25.0" y="43.30127">9</text>
        |      <text x="-50.0" y="0.0">13</text>
        |      <text x="50.0" y="0.0">2</text>
        |      <text x="-50.0" y="-50.0">12</text>
        |      <text x="118.30127" y="68.30127">7</text>
        |      <text x="25.0" y="43.30127">3</text>
        |      <text x="0.0" y="-50.0">11</text>
        |      <text x="168.30127" y="68.30127">8</text>
        |      <text x="93.30127" y="25.0">4</text>
        |    </g>
        |  </g>
        |  <metadata>
        |    <rdf:RDF>
        |      <cc:Work>
        |        <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>
        |        <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>
        |      </cc:Work>
        |    </rdf:RDF>
        |  </metadata>
        |</svg>""".stripMargin
  }

  it can "do the same but adding the polygons starting from the node after in perimeter" in {
    val t: Tiling =
      edges12Nodes8.maybeGrowNode(Node(1), vertex3344, FIXED(Node(2))).unsafe
    t.allLabels shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-93.30127 -68.30127 286.60254 161.60254" xmlns="http://www.w3.org/2000/svg">
        |  <g>
        |    <title>Tiling</title>
        |    <desc>Finite tessellation of regular polygons</desc>
        |    <g style="stroke:black">
        |      <title>Edges</title>
        |      <desc>Sides of the regular polygons</desc>
        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
        |      <line x1="0.0" y1="0.0" x2="25.0" y2="43.30127"/>
        |      <line x1="50.0" y1="0.0" x2="25.0" y2="43.30127"/>
        |      <line x1="50.0" y1="0.0" x2="93.30127" y2="25.0"/>
        |      <line x1="93.30127" y1="25.0" x2="68.30127" y2="68.30127"/>
        |      <line x1="25.0" y1="43.30127" x2="68.30127" y2="68.30127"/>
        |      <line x1="93.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
        |      <line x1="68.30127" y1="68.30127" x2="118.30127" y2="68.30127"/>
        |      <line x1="93.30127" y1="25.0" x2="143.30127" y2="25.0"/>
        |      <line x1="143.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
        |      <line x1="143.30127" y1="25.0" x2="168.30127" y2="68.30127"/>
        |      <line x1="118.30127" y1="68.30127" x2="168.30127" y2="68.30127"/>
        |      <line x1="25.0" y1="43.30127" x2="-18.30127" y2="68.30127"/>
        |      <line x1="-18.30127" y1="68.30127" x2="-43.30127" y2="25.0"/>
        |      <line x1="0.0" y1="0.0" x2="-43.30127" y2="25.0"/>
        |      <line x1="50.0" y1="0.0" x2="25.0" y2="-43.30127"/>
        |      <line x1="0.0" y1="0.0" x2="25.0" y2="-43.30127"/>
        |      <line x1="-43.30127" y1="25.0" x2="-68.30127" y2="-18.30127"/>
        |      <line x1="-68.30127" y1="-18.30127" x2="-25.0" y2="-43.30127"/>
        |      <line x1="0.0" y1="0.0" x2="-25.0" y2="-43.30127"/>
        |      <line x1="25.0" y1="-43.30127" x2="-25.0" y2="-43.30127"/>
        |    </g>
        |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="50.0,0.0 93.30127,25.0 143.30127,25.0 168.30127,68.30127 118.30127,68.30127 68.30127,68.30127 25.0,43.30127 -18.30127,68.30127 -43.30127,25.0 -68.30127,-18.30127 -25.0,-43.30127 25.0,-43.30127"/>
        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
        |      <title>Node labels</title>
        |      <desc>Each node showing its value</desc>
        |      <text x="68.30127" y="68.30127">5</text>
        |      <text x="-43.30127" y="25.0">10</text>
        |      <text x="0.0" y="0.0">1</text>
        |      <text x="143.30127" y="25.0">6</text>
        |      <text x="-18.30127" y="68.30127">9</text>
        |      <text x="-25.0" y="-43.30127">13</text>
        |      <text x="50.0" y="0.0">2</text>
        |      <text x="-68.30127" y="-18.30127">12</text>
        |      <text x="118.30127" y="68.30127">7</text>
        |      <text x="25.0" y="43.30127">3</text>
        |      <text x="25.0" y="-43.30127">11</text>
        |      <text x="168.30127" y="68.30127">8</text>
        |      <text x="93.30127" y="25.0">4</text>
        |    </g>
        |  </g>
        |  <metadata>
        |    <rdf:RDF>
        |      <cc:Work>
        |        <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>
        |        <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>
        |      </cc:Work>
        |    </rdf:RDF>
        |  </metadata>
        |</svg>""".stripMargin
  }

  val vertex43: Vertex =
    Vertex(Vector(4, 3).map(Polygon(_)))

  it can "return a new tiling with polygons added to partially fill a perimeter node" in {
    square.maybeGrowNode(Node(1), vertex43, BEFORE_PERIMETER).unsafe.allLabels shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-68.30127 -75.0 143.30127 150.0" xmlns="http://www.w3.org/2000/svg">
        |  <g>
        |    <title>Tiling</title>
        |    <desc>Finite tessellation of regular polygons</desc>
        |    <g style="stroke:black">
        |      <title>Edges</title>
        |      <desc>Sides of the regular polygons</desc>
        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
        |      <line x1="50.0" y1="0.0" x2="50.0" y2="50.0"/>
        |      <line x1="50.0" y1="50.0" x2="0.0" y2="50.0"/>
        |      <line x1="0.0" y1="0.0" x2="0.0" y2="50.0"/>
        |      <line x1="50.0" y1="0.0" x2="50.0" y2="-50.0"/>
        |      <line x1="50.0" y1="-50.0" x2="0.0" y2="-50.0"/>
        |      <line x1="0.0" y1="0.0" x2="0.0" y2="-50.0"/>
        |      <line x1="0.0" y1="-50.0" x2="-43.30127" y2="-25.0"/>
        |      <line x1="0.0" y1="0.0" x2="-43.30127" y2="-25.0"/>
        |    </g>
        |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 0.0,50.0 50.0,50.0 50.0,0.0 50.0,-50.0 0.0,-50.0 -43.30127,-25.0"/>
        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
        |      <title>Node labels</title>
        |      <desc>Each node showing its value</desc>
        |      <text x="50.0" y="-50.0">5</text>
        |      <text x="0.0" y="0.0">1</text>
        |      <text x="0.0" y="-50.0">6</text>
        |      <text x="50.0" y="0.0">2</text>
        |      <text x="-43.30127" y="-25.0">7</text>
        |      <text x="50.0" y="50.0">3</text>
        |      <text x="0.0" y="50.0">4</text>
        |    </g>
        |  </g>
        |  <metadata>
        |    <rdf:RDF>
        |      <cc:Work>
        |        <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>
        |        <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>
        |      </cc:Work>
        |    </rdf:RDF>
        |  </metadata>
        |</svg>""".stripMargin
  }

  it can "do the same but adding the partial polygons starting from the node after in perimeter" in {
    square.maybeGrowNode(Node(1), vertex43, AFTER_PERIMETER).unsafe.allLabels shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-75.0 -68.30127 150.0 143.30127" xmlns="http://www.w3.org/2000/svg">
        |  <g>
        |    <title>Tiling</title>
        |    <desc>Finite tessellation of regular polygons</desc>
        |    <g style="stroke:black">
        |      <title>Edges</title>
        |      <desc>Sides of the regular polygons</desc>
        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
        |      <line x1="50.0" y1="0.0" x2="50.0" y2="50.0"/>
        |      <line x1="50.0" y1="50.0" x2="0.0" y2="50.0"/>
        |      <line x1="0.0" y1="0.0" x2="0.0" y2="50.0"/>
        |      <line x1="0.0" y1="50.0" x2="-50.0" y2="50.0"/>
        |      <line x1="-50.0" y1="50.0" x2="-50.0" y2="0.0"/>
        |      <line x1="0.0" y1="0.0" x2="-50.0" y2="0.0"/>
        |      <line x1="-50.0" y1="0.0" x2="-25.0" y2="-43.30127"/>
        |      <line x1="0.0" y1="0.0" x2="-25.0" y2="-43.30127"/>
        |    </g>
        |    <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 50.0,0.0 50.0,50.0 0.0,50.0 -50.0,50.0 -50.0,0.0 -25.0,-43.30127"/>
        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
        |      <title>Node labels</title>
        |      <desc>Each node showing its value</desc>
        |      <text x="-50.0" y="50.0">5</text>
        |      <text x="0.0" y="0.0">1</text>
        |      <text x="-50.0" y="0.0">6</text>
        |      <text x="50.0" y="0.0">2</text>
        |      <text x="-25.0" y="-43.30127">7</text>
        |      <text x="50.0" y="50.0">3</text>
        |      <text x="0.0" y="50.0">4</text>
        |    </g>
        |  </g>
        |  <metadata>
        |    <rdf:RDF>
        |      <cc:Work>
        |        <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>
        |        <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>
        |      </cc:Work>
        |    </rdf:RDF>
        |  </metadata>
        |</svg>""".stripMargin
  }

  it can "NOT have polygons added with a total angle larger than the available" in {
    edges12Nodes8.maybeGrowNode(Node(1), Vertex(42, 42), BEFORE_PERIMETER) shouldBe
      Left(
        """Tiling cannot add polygons exceeding the node exterior angle:
          | 2.9919930034188504 is larger than 2.243994752564138 available at node 1.
          |See SVG:
          |<svg viewBox="-635.944897 -170.150273 829.246168 717.203632" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling with invalid addition</title>
          |    <desc>Adding exceeding polygons to node 1</desc>
          |    <g>
          |      <title>Tiling</title>
          |      <desc>Finite tessellation of regular polygons</desc>
          |      <g style="stroke:black">
          |        <title>Edges</title>
          |        <desc>Sides of the regular polygons</desc>
          |        <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
          |        <line x1="0.0" y1="0.0" x2="25.0" y2="43.30127"/>
          |        <line x1="50.0" y1="0.0" x2="25.0" y2="43.30127"/>
          |        <line x1="50.0" y1="0.0" x2="93.30127" y2="25.0"/>
          |        <line x1="93.30127" y1="25.0" x2="68.30127" y2="68.30127"/>
          |        <line x1="25.0" y1="43.30127" x2="68.30127" y2="68.30127"/>
          |        <line x1="93.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
          |        <line x1="68.30127" y1="68.30127" x2="118.30127" y2="68.30127"/>
          |        <line x1="93.30127" y1="25.0" x2="143.30127" y2="25.0"/>
          |        <line x1="143.30127" y1="25.0" x2="118.30127" y2="68.30127"/>
          |        <line x1="143.30127" y1="25.0" x2="168.30127" y2="68.30127"/>
          |        <line x1="118.30127" y1="68.30127" x2="168.30127" y2="68.30127"/>
          |        <line x1="25.0" y1="43.30127" x2="43.267051" y2="89.844958"/>
          |        <line x1="43.267051" y1="89.844958" x2="54.393098" y2="138.591353"/>
          |        <line x1="54.393098" y1="138.591353" x2="58.129603" y2="188.451543"/>
          |        <line x1="58.129603" y1="188.451543" x2="54.393098" y2="238.311733"/>
          |        <line x1="54.393098" y1="238.311733" x2="43.267051" y2="287.058129"/>
          |        <line x1="43.267051" y1="287.058129" x2="25.0" y2="333.601816"/>
          |        <line x1="25.0" y1="333.601816" x2="0.0" y2="376.903086"/>
          |        <line x1="0.0" y1="376.903086" x2="-31.17449" y2="415.99466"/>
          |        <line x1="-31.17449" y1="415.99466" x2="-67.827084" y2="450.003297"/>
          |        <line x1="-67.827084" y1="450.003297" x2="-109.139022" y2="478.1693"/>
          |        <line x1="-109.139022" y1="478.1693" x2="-154.187466" y2="499.863487"/>
          |        <line x1="-154.187466" y1="499.863487" x2="-201.966106" y2="514.601246"/>
          |        <line x1="-201.966106" y1="514.601246" x2="-251.407647" y2="522.053359"/>
          |        <line x1="-251.407647" y1="522.053359" x2="-301.407647" y2="522.053359"/>
          |        <line x1="-301.407647" y1="522.053359" x2="-350.849189" y2="514.601246"/>
          |        <line x1="-350.849189" y1="514.601246" x2="-398.627829" y2="499.863487"/>
          |        <line x1="-398.627829" y1="499.863487" x2="-443.676272" y2="478.1693"/>
          |        <line x1="-443.676272" y1="478.1693" x2="-484.988211" y2="450.003297"/>
          |        <line x1="-484.988211" y1="450.003297" x2="-521.640805" y2="415.99466"/>
          |        <line x1="-521.640805" y1="415.99466" x2="-552.815295" y2="376.903086"/>
          |        <line x1="-552.815295" y1="376.903086" x2="-577.815295" y2="333.601816"/>
          |        <line x1="-577.815295" y1="333.601816" x2="-596.082346" y2="287.058129"/>
          |        <line x1="-596.082346" y1="287.058129" x2="-607.208393" y2="238.311733"/>
          |        <line x1="-607.208393" y1="238.311733" x2="-610.944897" y2="188.451543"/>
          |        <line x1="-610.944897" y1="188.451543" x2="-607.208393" y2="138.591353"/>
          |        <line x1="-607.208393" y1="138.591353" x2="-596.082346" y2="89.844958"/>
          |        <line x1="-596.082346" y1="89.844958" x2="-577.815295" y2="43.30127"/>
          |        <line x1="-577.815295" y1="43.30127" x2="-552.815295" y2="0.0"/>
          |        <line x1="-552.815295" y1="0.0" x2="-521.640805" y2="-39.091574"/>
          |        <line x1="-521.640805" y1="-39.091574" x2="-484.988211" y2="-73.100211"/>
          |        <line x1="-484.988211" y1="-73.100211" x2="-443.676272" y2="-101.266214"/>
          |        <line x1="-443.676272" y1="-101.266214" x2="-398.627829" y2="-122.960401"/>
          |        <line x1="-398.627829" y1="-122.960401" x2="-350.849189" y2="-137.69816"/>
          |        <line x1="-350.849189" y1="-137.69816" x2="-301.407647" y2="-145.150273"/>
          |        <line x1="-301.407647" y1="-145.150273" x2="-251.407647" y2="-145.150273"/>
          |        <line x1="-251.407647" y1="-145.150273" x2="-201.966106" y2="-137.69816"/>
          |        <line x1="-201.966106" y1="-137.69816" x2="-154.187466" y2="-122.960401"/>
          |        <line x1="-154.187466" y1="-122.960401" x2="-109.139022" y2="-101.266214"/>
          |        <line x1="-109.139022" y1="-101.266214" x2="-67.827084" y2="-73.100211"/>
          |        <line x1="-67.827084" y1="-73.100211" x2="-31.17449" y2="-39.091574"/>
          |        <line x1="0.0" y1="0.0" x2="-31.17449" y2="-39.091574"/>
          |      </g>
          |      <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 -31.17449,-39.091574 -67.827084,-73.100211 -109.139022,-101.266214 -154.187466,-122.960401 -201.966106,-137.69816 -251.407647,-145.150273 -301.407647,-145.150273 -350.849189,-137.69816 -398.627829,-122.960401 -443.676272,-101.266214 -484.988211,-73.100211 -521.640805,-39.091574 -552.815295,0.0 -577.815295,43.30127 -596.082346,89.844958 -607.208393,138.591353 -610.944897,188.451543 -607.208393,238.311733 -596.082346,287.058129 -577.815295,333.601816 -552.815295,376.903086 -521.640805,415.99466 -484.988211,450.003297 -443.676272,478.1693 -398.627829,499.863487 -350.849189,514.601246 -301.407647,522.053359 -251.407647,522.053359 -201.966106,514.601246 -154.187466,499.863487 -109.139022,478.1693 -67.827084,450.003297 -31.17449,415.99466 0.0,376.903086 25.0,333.601816 43.267051,287.058129 54.393098,238.311733 58.129603,188.451543 54.393098,138.591353 43.267051,89.844958 25.0,43.30127 68.30127,68.30127 118.30127,68.30127 168.30127,68.30127 143.30127,25.0 93.30127,25.0 50.0,0.0"/>
          |      <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |        <title>Node labels</title>
          |        <desc>Each node showing its value</desc>
          |        <text x="68.30127" y="68.30127">5</text>
          |        <text x="54.393098" y="138.591353">10</text>
          |        <text x="25.0" y="333.601816">14</text>
          |        <text x="0.0" y="0.0">1</text>
          |        <text x="143.30127" y="25.0">6</text>
          |        <text x="43.267051" y="89.844958">9</text>
          |        <text x="50.0" y="0.0">2</text>
          |        <text x="54.393098" y="238.311733">12</text>
          |        <text x="168.30127" y="68.30127">8</text>
          |        <text x="93.30127" y="25.0">4</text>
          |        <text x="-301.407647" y="-145.150273">42</text>
          |        <text x="-398.627829" y="499.863487">24</text>
          |        <text x="-521.640805" y="-39.091574">37</text>
          |        <text x="-443.676272" y="478.1693">25</text>
          |        <text x="-201.966106" y="514.601246">20</text>
          |        <text x="-109.139022" y="-101.266214">46</text>
          |        <text x="-577.815295" y="333.601816">29</text>
          |        <text x="-251.407647" y="522.053359">21</text>
          |        <text x="-607.208393" y="138.591353">33</text>
          |        <text x="-552.815295" y="376.903086">28</text>
          |        <text x="-484.988211" y="-73.100211">38</text>
          |        <text x="43.267051" y="287.058129">13</text>
          |        <text x="-350.849189" y="-137.69816">41</text>
          |        <text x="-154.187466" y="-122.960401">45</text>
          |        <text x="-67.827084" y="450.003297">17</text>
          |        <text x="-610.944897" y="188.451543">32</text>
          |        <text x="-596.082346" y="89.844958">34</text>
          |        <text x="-301.407647" y="522.053359">22</text>
          |        <text x="-201.966106" y="-137.69816">44</text>
          |        <text x="-521.640805" y="415.99466">27</text>
          |        <text x="118.30127" y="68.30127">7</text>
          |        <text x="-443.676272" y="-101.266214">39</text>
          |        <text x="25.0" y="43.30127">3</text>
          |        <text x="-577.815295" y="43.30127">35</text>
          |        <text x="-31.17449" y="-39.091574">48</text>
          |        <text x="-109.139022" y="478.1693">18</text>
          |        <text x="-31.17449" y="415.99466">16</text>
          |        <text x="-607.208393" y="238.311733">31</text>
          |        <text x="58.129603" y="188.451543">11</text>
          |        <text x="-251.407647" y="-145.150273">43</text>
          |        <text x="-398.627829" y="-122.960401">40</text>
          |        <text x="-484.988211" y="450.003297">26</text>
          |        <text x="-350.849189" y="514.601246">23</text>
          |        <text x="-552.815295" y="0.0">36</text>
          |        <text x="-596.082346" y="287.058129">30</text>
          |        <text x="-154.187466" y="499.863487">19</text>
          |        <text x="-67.827084" y="-73.100211">47</text>
          |        <text x="0.0" y="376.903086">15</text>
          |      </g>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have a polygon added that touches a vertex of the existing perimeter" in {
    val start: Tiling =
      Tiling.maybe(commonEdges.dropRight(2)).unsafe
    start.maybeGrowNode(Node(8), Vertex(Vector(Polygon(4))), BEFORE_PERIMETER) shouldBe
      Left(
        """Tiling must have all perimeter nodes at different cartesian coords:
          | found invalid couple (10,11).
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
          |      <text x="0.0" y="136.60254">12</text>
          |      <text x="75.0" y="43.30127">7</text>
          |      <text x="-18.30127" y="68.30127">3</text>
          |      <text x="25.0" y="93.30127">11</text>
          |      <text x="-43.30127" y="111.60254">8</text>
          |      <text x="25.0" y="43.30127">4</text>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have a polygon added to a non existing node" in {
    Tiling.pattern_4444(2, 2).unsafe.maybeGrowNode(Node(100), Vertex(Polygon(4)), BEFORE_PERIMETER) shouldBe
      Left(
        """Tiling can add polygons only to perimeter nodes:
          | found unknown node 100.
          |See SVG:
          |<svg viewBox="-25.0 -125.0 150.0 150.0" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling with invalid addition</title>
          |    <desc>Adding to unknown node 100</desc>
          |    <g>
          |      <title>Tiling</title>
          |      <desc>Finite tessellation of regular polygons</desc>
          |      <g style="stroke:black">
          |        <title>Edges</title>
          |        <desc>Sides of the regular polygons</desc>
          |        <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
          |        <line x1="0.0" y1="-50.0" x2="50.0" y2="-50.0"/>
          |        <line x1="0.0" y1="-100.0" x2="50.0" y2="-100.0"/>
          |        <line x1="50.0" y1="0.0" x2="100.0" y2="0.0"/>
          |        <line x1="50.0" y1="-50.0" x2="100.0" y2="-50.0"/>
          |        <line x1="50.0" y1="-100.0" x2="100.0" y2="-100.0"/>
          |        <line x1="0.0" y1="0.0" x2="0.0" y2="-50.0"/>
          |        <line x1="0.0" y1="-50.0" x2="0.0" y2="-100.0"/>
          |        <line x1="50.0" y1="0.0" x2="50.0" y2="-50.0"/>
          |        <line x1="50.0" y1="-50.0" x2="50.0" y2="-100.0"/>
          |        <line x1="100.0" y1="0.0" x2="100.0" y2="-50.0"/>
          |        <line x1="100.0" y1="-50.0" x2="100.0" y2="-100.0"/>
          |      </g>
          |      <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 50.0,0.0 100.0,0.0 100.0,-50.0 100.0,-100.0 50.0,-100.0 0.0,-100.0 0.0,-50.0"/>
          |      <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |        <title>Node labels</title>
          |        <desc>Each node showing its value</desc>
          |        <text x="50.0" y="-50.0">5</text>
          |        <text x="0.0" y="0.0">1</text>
          |        <text x="100.0" y="-50.0">6</text>
          |        <text x="100.0" y="-100.0">9</text>
          |        <text x="50.0" y="0.0">2</text>
          |        <text x="0.0" y="-100.0">7</text>
          |        <text x="100.0" y="0.0">3</text>
          |        <text x="50.0" y="-100.0">8</text>
          |        <text x="0.0" y="-50.0">4</text>
          |      </g>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

  it can "NOT have a polygon added to a non perimeter node" in {
    Tiling.pattern_4444(2, 2).unsafe.maybeGrowNode(Node(5), Vertex(Polygon(4)), BEFORE_PERIMETER) shouldBe
      Left(
        """Tiling can add polygons only to perimeter nodes:
          | found inner node 5.
          |See SVG:
          |<svg viewBox="-25.0 -125.0 150.0 150.0" xmlns="http://www.w3.org/2000/svg">
          |  <g>
          |    <title>Tiling with invalid addition</title>
          |    <desc>Adding to inner node 5</desc>
          |    <g style="fill:red;stroke:orangered">
          |      <title>Highlighted</title>
          |      <desc>Nodes</desc>
          |      <circle cx="50.0" cy="-50.0" r="5.0"/>
          |    </g>
          |    <g>
          |      <title>Tiling</title>
          |      <desc>Finite tessellation of regular polygons</desc>
          |      <g style="stroke:black">
          |        <title>Edges</title>
          |        <desc>Sides of the regular polygons</desc>
          |        <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
          |        <line x1="0.0" y1="-50.0" x2="50.0" y2="-50.0"/>
          |        <line x1="0.0" y1="-100.0" x2="50.0" y2="-100.0"/>
          |        <line x1="50.0" y1="0.0" x2="100.0" y2="0.0"/>
          |        <line x1="50.0" y1="-50.0" x2="100.0" y2="-50.0"/>
          |        <line x1="50.0" y1="-100.0" x2="100.0" y2="-100.0"/>
          |        <line x1="0.0" y1="0.0" x2="0.0" y2="-50.0"/>
          |        <line x1="0.0" y1="-50.0" x2="0.0" y2="-100.0"/>
          |        <line x1="50.0" y1="0.0" x2="50.0" y2="-50.0"/>
          |        <line x1="50.0" y1="-50.0" x2="50.0" y2="-100.0"/>
          |        <line x1="100.0" y1="0.0" x2="100.0" y2="-50.0"/>
          |        <line x1="100.0" y1="-50.0" x2="100.0" y2="-100.0"/>
          |      </g>
          |      <polygon style="fill:none;stroke:blue;stroke-width:2" points="0.0,0.0 50.0,0.0 100.0,0.0 100.0,-50.0 100.0,-100.0 50.0,-100.0 0.0,-100.0 0.0,-50.0"/>
          |      <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
          |        <title>Node labels</title>
          |        <desc>Each node showing its value</desc>
          |        <text x="50.0" y="-50.0">5</text>
          |        <text x="0.0" y="0.0">1</text>
          |        <text x="100.0" y="-50.0">6</text>
          |        <text x="100.0" y="-100.0">9</text>
          |        <text x="50.0" y="0.0">2</text>
          |        <text x="0.0" y="-100.0">7</text>
          |        <text x="100.0" y="0.0">3</text>
          |        <text x="50.0" y="-100.0">8</text>
          |        <text x="0.0" y="-50.0">4</text>
          |      </g>
          |    </g>
          |  </g>
          |</svg>""".stripMargin
      )
  }

}
