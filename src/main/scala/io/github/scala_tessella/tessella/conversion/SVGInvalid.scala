package io.github.scala_tessella.tessella
package conversion

import ConverterSVG.*
import TilingCoordinates.toBox
import Geometry.{Box, LineSegment, Point}
import SharedML.*
import SVG.*

import scala.xml.Elem

/** Methods to highlight `Tiling` invalidating issues into an SVG file */
object SVGInvalid extends ConverterSVG:

  private val intersectionStyle: Style =
    Style(List(
      fill("red"),
      stroke("orangered")
    ) *)

  /** `circle` element for invalid node
   *
   * @param center spatial requirement
   */
  def invalidNode(center: Point): Elem =
    circle(center, 0.1)

  /** `group` titled and styled for perimeter intersections
   *
   * @param intersections elements
   */
  def intersectionsGroup(intersections: Seq[Elem]): Elem =
    group(
      Option(Title("Perimeter intersections")),
      Option(Description("Perimeter edges intersecting")),
      intersections *
    )
      .withStyle(intersectionStyle)

  /** `group` titled and styled for invalid edges
   *
   * @param lines elements
   * @param width of `stroke-width` attribute
   */
  private def invalidEdgesGroup(lines: Seq[Elem], width: Int = 1): Elem =
    group(
      Option(Title("Highlighted")),
      Option(Description("Edges")),
      lines *
    )
      .withStyle(Style(
        List(
          stroke("red"),
          strokeWidth(width)
        ) *
      ))

  /** `group` titled and styled for an invalid node
   *
   * @param center spatial coordinates
   */
  def invalidNodeGroup(center: Point): Elem =
    group(
      Option(Title("Highlighted")),
      Option(Description("Nodes")),
      List(circle(center, 0.1)) *
    )
      .withStyle(intersectionStyle)

  /** `group` for an invalid edge
   *
   * @param segment spatial coordinates
   * @param width of `stroke-width` attribute
   */
  def invalidEdgeGroup(segment: LineSegment, width: Int = 1): Elem =
    invalidEdgesGroup(List(line(segment)), width)

  private def invalidPretty(elems: List[Elem], title: Title, desc: Description, box: Box) =
    prettyPrinter.format(svg(
      box,
      group(Option(title), Option(desc), elems *)
    ))

  extension (tiling: Tiling)

    /** Prettified `svg` showing invalid perimeter
     *
     * @param desc description
     * @param intersections optional element
     */
    def invalidPerimeterSVG(desc: Description, intersections: Option[Elem] = None): String =
      val toLabelsSVG: Elem =
        perimeterLabelsGroup(tiling.perimeterCoords.map((node, coordinate) => node.label(coordinate)).toSeq)
      val elems: List[Elem] =
        polygon(tiling.perimeterPoints).withStyle(perimeterStyle)
          :: (intersections match
          case Some(perimeterIntersections) => List(perimeterIntersections, toLabelsSVG)
          case _                            => List(toLabelsSVG)
          )
      invalidPretty(
        elems,
        Title("Tiling perimeter"),
        desc,
        tiling.perimeter.toRingEdges.toList.toBox(tiling.perimeterCoords)
      )

    /** Prettified `svg` showing invalid tiling addition
     *
     * @param desc description
     * @param invalid optional element
     */
    def invalidTilingSVG(desc: Description, invalid: Option[Elem] = None): String =
      val elems =
        (invalid match
          case Some(invalidAddition) => List(invalidAddition)
          case _                     => Nil
        ) :+ tiling.tessellationGroup(labelledNodes = LabelledNodes.ALL)
      invalidPretty(
        elems,
        Title("Tiling with invalid addition"),
        desc,
        tiling.toBox
      )
