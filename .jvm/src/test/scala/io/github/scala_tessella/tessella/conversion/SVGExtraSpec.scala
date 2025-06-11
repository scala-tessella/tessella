package io.github.scala_tessella.tessella
package conversion

import SVG.*
import SVGExtra.*
import Outliers.p666_triangle

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class SVGExtraSpec extends AnyFlatSpec with Helper with should.Matchers:

  "A tiling" can "be concentric" in {
    prettyPrinter.format(
      p666_triangle.toNestedPerimetersSVG()
    ) shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-50.0 -25.0 375.0 396.410162" xmlns="http://www.w3.org/2000/svg">
        |  <g style="stroke:purple;stroke-width:5;fill:none">
        |    <title>Nested</title>
        |    <desc>Nested perimeters</desc>
        |    <g style="fill-opacity:0.5">
        |      <g>
        |        <polygon style="fill:red" points="0.0,0.0 -25.0,43.30127 0.0,86.60254 -25.0,129.903811 0.0,173.205081 -25.0,216.506351 0.0,259.807621 -25.0,303.108891 0.0,346.410162 50.0,346.410162 75.0,303.108891 125.0,303.108891 150.0,259.807621 200.0,259.807621 225.0,216.506351 275.0,216.506351 300.0,173.205081 275.0,129.903811 225.0,129.903811 200.0,86.60254 150.0,86.60254 125.0,43.30127 75.0,43.30127 50.0,0.0"/>
        |      </g>
        |    </g>
        |    <g style="fill-opacity:0.5">
        |      <g>
        |        <polygon style="fill:red" points="150.0,173.205081 125.0,129.903811 75.0,129.903811 50.0,173.205081 75.0,216.506351 125.0,216.506351"/>
        |      </g>
        |    </g>
        |  </g>
        |  <g>
        |    <title>Tiling</title>
        |    <desc>Finite tessellation of regular polygons</desc>
        |    <g style="stroke:black">
        |      <title>Edges</title>
        |      <desc>Sides of the regular polygons</desc>
        |      <line x1="-25.0" y1="43.30127" x2="0.0" y2="86.60254"/>
        |      <line x1="-25.0" y1="129.903811" x2="0.0" y2="173.205081"/>
        |      <line x1="-25.0" y1="216.506351" x2="0.0" y2="259.807621"/>
        |      <line x1="-25.0" y1="303.108891" x2="0.0" y2="346.410162"/>
        |      <line x1="0.0" y1="0.0" x2="50.0" y2="0.0"/>
        |      <line x1="0.0" y1="86.60254" x2="50.0" y2="86.60254"/>
        |      <line x1="0.0" y1="173.205081" x2="50.0" y2="173.205081"/>
        |      <line x1="0.0" y1="259.807621" x2="50.0" y2="259.807621"/>
        |      <line x1="0.0" y1="346.410162" x2="50.0" y2="346.410162"/>
        |      <line x1="50.0" y1="0.0" x2="75.0" y2="43.30127"/>
        |      <line x1="50.0" y1="86.60254" x2="75.0" y2="129.903811"/>
        |      <line x1="50.0" y1="173.205081" x2="75.0" y2="216.506351"/>
        |      <line x1="50.0" y1="259.807621" x2="75.0" y2="303.108891"/>
        |      <line x1="75.0" y1="43.30127" x2="125.0" y2="43.30127"/>
        |      <line x1="75.0" y1="129.903811" x2="125.0" y2="129.903811"/>
        |      <line x1="75.0" y1="216.506351" x2="125.0" y2="216.506351"/>
        |      <line x1="75.0" y1="303.108891" x2="125.0" y2="303.108891"/>
        |      <line x1="125.0" y1="43.30127" x2="150.0" y2="86.60254"/>
        |      <line x1="125.0" y1="129.903811" x2="150.0" y2="173.205081"/>
        |      <line x1="125.0" y1="216.506351" x2="150.0" y2="259.807621"/>
        |      <line x1="150.0" y1="86.60254" x2="200.0" y2="86.60254"/>
        |      <line x1="150.0" y1="173.205081" x2="200.0" y2="173.205081"/>
        |      <line x1="150.0" y1="259.807621" x2="200.0" y2="259.807621"/>
        |      <line x1="200.0" y1="86.60254" x2="225.0" y2="129.903811"/>
        |      <line x1="200.0" y1="173.205081" x2="225.0" y2="216.506351"/>
        |      <line x1="225.0" y1="129.903811" x2="275.0" y2="129.903811"/>
        |      <line x1="225.0" y1="216.506351" x2="275.0" y2="216.506351"/>
        |      <line x1="275.0" y1="129.903811" x2="300.0" y2="173.205081"/>
        |      <line x1="0.0" y1="0.0" x2="-25.0" y2="43.30127"/>
        |      <line x1="0.0" y1="86.60254" x2="-25.0" y2="129.903811"/>
        |      <line x1="0.0" y1="173.205081" x2="-25.0" y2="216.506351"/>
        |      <line x1="0.0" y1="259.807621" x2="-25.0" y2="303.108891"/>
        |      <line x1="75.0" y1="43.30127" x2="50.0" y2="86.60254"/>
        |      <line x1="75.0" y1="129.903811" x2="50.0" y2="173.205081"/>
        |      <line x1="75.0" y1="216.506351" x2="50.0" y2="259.807621"/>
        |      <line x1="75.0" y1="303.108891" x2="50.0" y2="346.410162"/>
        |      <line x1="150.0" y1="86.60254" x2="125.0" y2="129.903811"/>
        |      <line x1="150.0" y1="173.205081" x2="125.0" y2="216.506351"/>
        |      <line x1="150.0" y1="259.807621" x2="125.0" y2="303.108891"/>
        |      <line x1="225.0" y1="129.903811" x2="200.0" y2="173.205081"/>
        |      <line x1="225.0" y1="216.506351" x2="200.0" y2="259.807621"/>
        |      <line x1="300.0" y1="173.205081" x2="275.0" y2="216.506351"/>
        |    </g>
        |    <g style="fill:#4a4a4a;text-anchor:middle;font-family:Arial,Helvetica,sans-serif">
        |      <title>Node labels</title>
        |      <desc>Each node showing its value</desc>
        |      <text x="150.0" y="86.60254">5</text>
        |      <text x="-25.0" y="43.30127">10</text>
        |      <text x="125.0" y="129.903811">14</text>
        |      <text x="0.0" y="0.0">1</text>
        |      <text x="200.0" y="86.60254">6</text>
        |      <text x="300.0" y="173.205081">9</text>
        |      <text x="75.0" y="129.903811">13</text>
        |      <text x="50.0" y="0.0">2</text>
        |      <text x="50.0" y="86.60254">12</text>
        |      <text x="225.0" y="129.903811">7</text>
        |      <text x="75.0" y="43.30127">3</text>
        |      <text x="275.0" y="216.506351">18</text>
        |      <text x="0.0" y="86.60254">11</text>
        |      <text x="275.0" y="129.903811">8</text>
        |      <text x="125.0" y="43.30127">4</text>
        |      <text x="150.0" y="173.205081">15</text>
        |      <text x="150.0" y="259.807621">24</text>
        |      <text x="200.0" y="259.807621">25</text>
        |      <text x="0.0" y="173.205081">20</text>
        |      <text x="75.0" y="303.108891">29</text>
        |      <text x="50.0" y="259.807621">28</text>
        |      <text x="50.0" y="173.205081">21</text>
        |      <text x="50.0" y="346.410162">33</text>
        |      <text x="0.0" y="346.410162">32</text>
        |      <text x="225.0" y="216.506351">17</text>
        |      <text x="75.0" y="216.506351">22</text>
        |      <text x="0.0" y="259.807621">27</text>
        |      <text x="200.0" y="173.205081">16</text>
        |      <text x="-25.0" y="303.108891">31</text>
        |      <text x="-25.0" y="216.506351">26</text>
        |      <text x="125.0" y="216.506351">23</text>
        |      <text x="125.0" y="303.108891">30</text>
        |      <text x="-25.0" y="129.903811">19</text>
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
