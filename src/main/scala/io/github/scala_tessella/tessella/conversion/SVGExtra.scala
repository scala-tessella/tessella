package io.github.scala_tessella.tessella
package conversion

import ConverterSVG.*
import GeometryBase.SimplePolygon9D
import SharedML.{Attribute, Style, Title, withStyle}
import SVG.*
import Topology.{Edge, Node}

import scala.xml.Elem
import scala.jdk.CollectionConverters.*

/** Extra methods to highlight specific `Tiling` features into an SVG file. */
object SVGExtra:

  private def fillRed: Style =
    Style(fill("red"))

  extension (shapes: Vector[Elem])

    def toXcentricGroup(title: Title, desc: Description): Elem =
      group(
        Option(title),
        Option(desc),
        shapes *
      )
        .withStyle(
          Style(
            stroke("purple"),
            strokeWidth(5.0),
            fill("none")
          )
        )

    def addOpacity(extra: Int = 1): Vector[Elem] =
      val opacity: Double =
        1.0 / (shapes.size - extra)
      shapes.map(
        _.withStyle(
          Style(Attribute.create("fill-opacity")(opacity.toString))
        )
      )

    def intoGroups: Vector[Elem] =
      shapes.map(elem => group(None, None, elems = elem))

  extension (tiling: Tiling)

//    private def toElemFrom(path: tiling.Path, allRings: Boolean = false): Elem =
//      if path.hasJustOneNode then
//        circle(tiling.coords(path.toNodes.head), 0.05)
//          .withStyle(fillRed)
//      else if allRings then
//        polygon(path.toNodes.map(tiling.coords))
//          .withStyle(fillRed)
//      else if path.isLoop then
//        polygon(path.toNodes.tail.map(tiling.coords))
//          .withStyle(fillRed)
//      else
//        polyline(path.toNodes.map(tiling.coords))

    private def toElemFrom2(ringPath: tiling.RingPath): Elem =
      polygon(ringPath.toRingNodes.map(tiling.coords))
        .withStyle(fillRed)

    //          .withStyle(Style(Attribute.create("stroke-dasharray")("10,5")))

    private def rawPerimetersSVG(ringSets: List[List[tiling.RingPath]],
                                 specificLevels: Option[List[Int]],
                                 title: Title,
                                 description: Description): Elem =
      val elements: Vector[Elem] =
        ringSets
          .map(_.map(toElemFrom2).toVector)
          .map(elems => group(None, None, elems *))
          .toVector
          .intoGroups
          .addOpacity(0)
      val filtered: Vector[Elem] =
        specificLevels match
          case None => elements
          case Some(levels) => levels.toVector.intersect(elements.indices).map(elements.reverse).reverse
      tiling.toViewBox(
        filtered.toXcentricGroup(title, description),
        tiling.tessellationGroup(showPerimeter = false, labelledNodes = LabelledNodes.ALL)
      )

    def toNestedPerimetersSVG(isStrict: Boolean = true, specificLevels: Option[List[Int]] = None): Elem =
      rawPerimetersSVG(
        tiling.nestedPerimeters(isStrict),
        specificLevels,
        Title("Nested"),
        Description("Nested perimeters")
      )

    def toOuterPerimetersSVG(origin: Node, isStrict: Boolean = true, specificLevels: Option[List[Int]] = None): Elem =
      rawPerimetersSVG(
        tiling.outerPerimetersFromSingle(origin, isStrict),
        specificLevels,
        Title("Outer"),
        Description("Outer perimeters")
      )

    def toOuterPolygonsSVG(origin: Node, isStrict: Boolean = true, specificLevels: Option[List[Int]] = None): Elem =
      val elements: Vector[Elem] =
//        val polygons = tiling.outerOrderedPolygonsFromSingle(origin, isStrict).map(_._1).toVector
        val polygons = tiling.outerPolygonsFromSingle(origin, isStrict).toVector
        polygons.indices.toVector
          .map(index =>
            group(None, None, polygons(index)
              .map(tiling.RingPath.simpleFromEdges)
              .map(_.toRingNodes.map(tiling.coords).toList)
              .map(SimplePolygon9D(_))
              .map(polygon(_).withStyle(
                Style(
                  fill(fillUniform(index % 5)),
//                  Attribute.create("fill-opacity")((1.0 / (index + 1)).toString),
                )
              ))
            *)
          )
      val filtered: Vector[Elem] =
        specificLevels match
          case None         => elements
          case Some(levels) => levels.toVector.intersect(elements.indices).map(elements.reverse).reverse
      tiling.toViewBox(
        filtered.toXcentricGroup(Title("Outcentric"), Description("Outcentric polygons")),
        tiling.tessellationGroup(showPerimeter = false, labelledNodes = LabelledNodes.ALL)
      )
