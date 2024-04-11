package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU_6
import RegularPolygon.Vertex
import Topology.*
import conversion.SVG.*
import conversion.SVGExtra.*
import utility.Utils.compareElems

import scala.xml.{Elem, XML}

trait Helper:

  val S6: Double =
    Math.sin(TAU_6.toDouble)

  val commonEdges: List[Edge] =
    List(1--2, 2--3, 3--4, 4--1, 2--5, 5--3, 1--6, 4--6, 7--6, 7--4, 8--5, 8--3, 9--7, 10--9, 10--4, 11--8, 12--3)

  lazy val triangle: Tiling =
    Tiling.fromPolygon(3)

  lazy val hexagonTriangles: Tiling =
    Tiling.fromVertex(Vertex(3, 3, 3, 3, 3, 3))

  lazy val square: Tiling =
    Tiling.fromPolygon(4)

  lazy val pentagon: Tiling =
    Tiling.fromPolygon(5)

  lazy val hexagon: Tiling =
    Tiling.fromPolygon(6)

  lazy val octagon: Tiling =
    Tiling.fromPolygon(8)

  lazy val dodecagon: Tiling =
    Tiling.fromPolygon(12)

  lazy val sixSquares: Tiling =
    Tiling.squareNet(3, 2).unsafe

  extension (maybe: Either[_, Tiling])

    def unsafe: Tiling =
      maybe.toOption.get

  extension (tiling: Tiling)

    def allLabels: String =
      prettyPrinter.format(tiling.toSVG(labelledNodes = LabelledNodes.ALL))

    def withPolygons: String =
      prettyPrinter.format(tiling.toSVG(fillPolygons = true))

    def withGonality: String =
      prettyPrinter.format(tiling.toSVG(fillPolygons = true, markStyle = MarkStyle.GONALITY))

    def withUniformity: String =
      prettyPrinter.format(tiling.toSVG(fillPolygons = true, markStyle = MarkStyle.UNIFORMITY))

    def withUniformityAndAllLabels: String =
      prettyPrinter.format(tiling.toSVG(labelledNodes = LabelledNodes.ALL, fillPolygons = true, markStyle = MarkStyle.UNIFORMITY))

    def nestedPerimetersSVG: String =
      prettyPrinter.format(tiling.toNestedPerimetersSVG())

  extension (coords: Coords)

    def almostEqualsMap(others: Coords): Boolean =
      coords.compareElems(others)((l1, l2) => l1._1 == l2._1 && l1._2.almostEquals(l2._2, LESSER_ACCURACY))

  def squareReticulate(side: Int): Tiling =
    Tiling.squareNet(side, side).unsafe
  
  private def saveFile(elem: Elem, filename: String)(extension: String): Unit =
    XML.save(
      s"src/test/resources/$filename.$extension",
      elem,
      "UTF-8",
      xmlDecl = extension match
        case "svg"  => true
        case "html" => false
        case _      => throw new Error
    )

  def saveFileSVG(elem: Elem, filename: String): Unit =
    saveFile(elem, filename)("svg")

  def saveFileHTML(elem: Elem, filename: String): Unit =
    saveFile(elem, filename)("html")
