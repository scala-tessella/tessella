package io.github.scala_tessella.tessella
package conversion

import ConverterSVG.*
import SVG.LabelledNodes.PERIMETER_ONLY
import SVG.MarkStyle.NONE
import SharedML.*
import RegularPolygon.Vertex
import Topology.{Edge, Node, NodeOrdering}
import TilingUniformity.groupUniformsNestedComplete
import utility.Utils.toCouple
import utility.UtilsOption.getDefined

import io.github.scala_tessella.ring_seq.RingSeq.Index
import math.geom2d.Point2D
import math.geom2d.line.LineSegment2D
import math.geom2d.polygon.SimplePolygon2D

import scala.jdk.CollectionConverters.*
import scala.xml.Elem

/** Methods to convert a `Tiling` into an SVG file */
object SVG extends ConverterSVG:

  /** Alternative options for displaying node labels */
  enum LabelledNodes:

    case NONE           extends LabelledNodes
    case PERIMETER_ONLY extends LabelledNodes
    case ALL            extends LabelledNodes

  /** Alternative options for displaying node marks */
  enum MarkStyle:

    case NONE       extends MarkStyle
    case UNIFORMITY extends MarkStyle
    case GONALITY   extends MarkStyle

  private val fillMap: Map[Int, String] =
    Map(
      3  -> "yellow",
      4  -> "goldenrod",
      5  -> "hotpink",
      6  -> "grey",
      7  -> "orange",
      8  -> "coral",
      9  -> "darkgray",
      10 -> "lightpink",
      12 -> "lightgreen",
      15 -> "darkseagreen",
      18 -> "olivedrab",
      20 -> "olive",
      24 -> "palegreen",
      42 -> "darkolivegreen"
    )

  /** Colors to differentiate uniformity marks*/
  val fillUniform: IndexedSeq[String] =
    IndexedSeq("blue", "red", "green", "white", "black")

  extension (double: Double)

    private def toShortString: String =
      val int = double.toInt
      if double == int then int.toString else double.toString

  /** `stroke-width` attribute */
  val strokeWidth: Double => Attribute =
    width => Attribute.create("stroke-width")(width.toShortString)

  /** `stroke-opacity` attribute */
  private val strokeOpacity: Int => Attribute =
    opacity => Attribute.create("stroke-opacity")(opacity.toString)

  val perimeterStyle: Style =
    Style(List(
      fill("none"),
      stroke("blue"),
      strokeWidth(2)
    ) *)

  private val inversionStyle: Style =
    Style(List(
      fill("none"),
      stroke("red"),
      strokeWidth(1)
    ) *)

  private val labelStyle: Style =
    Style(List(
      fill("#4a4a4a"),
      Attribute.create("text-anchor")("middle"),
      Attribute.create("font-family")("Arial,Helvetica,sans-serif")
    ) *)

  private val noStrokeStyle: Style =
    Style(stroke("none"))

  private def fillPolygonStyle(size: Int): Style =
    Style(fill(s"${fillMap(size)}"))

  private def fillVertexStyle(vertex: Vertex): Style =
    val color =
      FullVertex.maybe(vertex.toPolygons).toOption.map(_.toString).getOrElse("") match
        case "(6³)"       => "yellow"
        case "(4⁴)"       => "hotpink"
        case "(3⁶)"       => "grey"
        case "(3⁴.6)"     => "coral"
        case "(3³.4²)"    => "darkgreen"
        case "(3².4.3.4)" => "fuchsia"
        case "(3.4.6.4)"  => "dodgerblue"
        case "(3.6.3.6)"  => "goldenrod"
        case "(3.12²)"    => "orchid"
        case "(4.6.12)"   => "tomato"
        case "(4.8²)"     => "palegreen"
        case "(3².4.12)"  => "indigo"
        case "(3².6²)"    => "pink"
        case "(3.4.3.12)" => "red"
        case "(3.4².6)"   => "lightgreen"
        case _            => "green"
    Style(List(
      stroke(color),
      strokeWidth(1),
      strokeOpacity(0),
      fill(color)
    ) *)

  val polylineStyle: Style =
    Style(List(
      stroke("red"),
      strokeWidth(1.5),
      Attribute.create("stroke-dasharray")("5,5"),
      fill("none")
    ) *)

  val arrowHeadStyle: Style =
    Style(List(
      stroke("red"),
      strokeWidth(1.5),
      fill("red")
    )*)

  private def markNode(point2D: Point2D): Elem =
    circle(point2D, 0.075)

  private def graphGroup(lines: Seq[Elem]): Elem =
    group(
      Option(Title("Edges")),
      Option(Description("Sides of the regular polygons")),
      lines *
    )
      .withStyle(Style(stroke("black")))

  private def labelsGroup(title: Option[Title], desc: Option[Description], labels: Seq[Elem]): Elem =
    group(title, desc, labels *).withStyle(labelStyle)

  /** `group` of the perimeter node labels */
  def perimeterLabelsGroup(labels: Seq[Elem]): Elem =
    labelsGroup(
      Option(Title("Perimeter node labels")),
      Option(Description("Each perimeter node showing its value")),
      labels
    )

  private def allLabelsGroup(labels: Seq[Elem]): Elem =
    labelsGroup(
      Option(Title("Node labels")),
      Option(Description("Each node showing its value")),
      labels
    )

  private def sameGonalityCirclesGroup(circles: Seq[Elem], vertex: Vertex): Elem =
    group(
      Option(Title(vertex.toString)),
      Option(Description(s"Vertices with $vertex pattern")),
      circles *
    )
      .withStyle(fillVertexStyle(vertex))

  private def allGonalsGroup(shapes: Seq[Elem]): Elem =
    group(
      Option(Title("Gonality")),
      Option(Description("Node marks by vertex pattern")),
      shapes *
    )
      .withStyle(noStrokeStyle)

  private def allUniformsGroup(shapes: Seq[Elem]): Elem =
    group(
      Option(Title("Uniformity")),
      Option(Description("Node marks by uniform vertex pattern")),
      shapes *
    )
      .withStyle(noStrokeStyle)

  private def uniformGroup(circles: Seq[Elem], index: Index, isMono: Boolean = false): Elem =
    val attributes: List[Attribute] =
      List(
        strokeOpacity(1),
        //          Attribute.create("stroke-dasharray", s"$index"),
        strokeWidth(2.5)
      )
    group(
      Option(Title(s"Uniform vertices set #${index + 1}")),
      None,
      circles *
    )
      .withStyle(Style(
        (
          if isMono then attributes 
          else fill(fillUniform(index)) :: attributes
         ) *
      ))

  private def sameSizePolygonsGroup(polygons: Seq[Elem], size: Int): Elem =
    group(
      Option(Title(s"Regular $size-gons")),
      Option(Description(s"Regular polygons with $size sides")),
      polygons *
    )
      .withStyle(fillPolygonStyle(size))

  private def allPolygonsGroup(sameSizeGroups: Seq[Elem]): Elem =
    group(
      Option(Title("Polygons")),
      Option(Description("Regular polygons")),
      sameSizeGroups *
    )
      .withStyle(noStrokeStyle)

  /** Attributes for metadata */
  val rdfAttributes: Seq[Attribute] =
    Seq(
      Attribute.create("xmlns:rdf")("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
      Attribute.create("xmlns:cc")("http://creativecommons.org/ns#"),
      Attribute.create("xmlns:dc")("http://purl.org/dc/elements/1.1/")
    )

  private def rdf(elems: Elem*): Elem =
    <rdf:RDF>{ elems.toNodeBuffer }</rdf:RDF>

  private def work(elems: Elem*): Elem =
    <cc:Work>{ elems.toNodeBuffer }</cc:Work>

  private def source: Elem =
    <dc:source rdf:resource="https://github.com/scala-tessella/tessella">Tessella</dc:source>

  private def license: Elem =
    <cc:license rdf:resource="https://www.apache.org/licenses/LICENSE-2.0"/>

  private def signatureMetadata: Elem =
    metadata(rdf(work(Seq(source, license) *)))

  extension (node: Node)

    /** Node label at given coordinates */
    def label(point2D: Point2D): Elem =
      text(point2D, node.toString)

  extension (tiling: Tiling)

    private def graphSVG: Option[Elem] =
      Option(graphGroup(tiling.edges.map(edge => line(tiling.coords(edge.lesserNode), tiling.coords(edge.greaterNode)))))

    private def polygonsSVG(fillPolygons: Boolean): Option[Elem] =
      if fillPolygons then
        val shapes =
          tiling.orientedPolygons.map(_.toPolygonPathNodes).groupBy(_.size).map((size, polygons) =>
            sameSizePolygonsGroup(
              polygons.map(nodes => polygon(nodes.map(node => tiling.coords(node)))),
              size
            )
          )
        Option(allPolygonsGroup(shapes.toSeq))
      else
        None

    private def perimeterSVG(showPerimeter: Boolean ): Option[Elem] =
      if showPerimeter then
        Option(polygon(tiling.perimeter.toRingNodes.map(tiling.coords)).withStyle(perimeterStyle))
      else
        None

    private def labelsSVG(labelledNodes: LabelledNodes): Option[Elem] =
      labelledNodes.ordinal match
        case 2 => Option(allLabelsGroup(tiling.coords.map((node, coordinates) => node.label(coordinates)).toSeq))
        case 1 => Option(perimeterLabelsGroup(tiling.perimeter.toRingNodes.map(node => node.label(tiling.coords(node)))))
        case _ => None

    private def marksSVG(markStyle: MarkStyle): Option[Elem] =
      markStyle.ordinal match
        case 2 =>
          Option(
            allGonalsGroup(
              tiling.groupGonals.map((vertex, nodes) =>
                sameGonalityCirclesGroup(
                  nodes.map(node => markNode(tiling.coords(node))).toSeq,
                  vertex
                )
              ).toSeq
            )
          )
        case 1 =>
          Option(
            allUniformsGroup(
              tiling.groupUniformsNestedComplete.map((vertex, sets) =>
                sameGonalityCirclesGroup(
                  sets.indices.map(index =>
                    uniformGroup(
                      sets(index)._2.map(node => markNode(tiling.coords(node))),
                      index,
                      sets.size == 1
                    )
                  ),
                  vertex
                )
              ).toSeq
            )
          )
        case _ => None

    private def growthSVG(showGrowth: Boolean): Option[Elem] =
      if showGrowth then
        val nodes: List[Int] =
          (1 to tiling.graphNodes.max(NodeOrdering).toInt).toList
        Option(animatedPolylineArrow(nodes.map(node => tiling.coords(Node(node))), arrowHeadStyle, polylineStyle))
      else
        None

    private def polygonsCentreCoords: Map[List[Edge], Point2D] =
      tiling.orientedPolygons.map(path =>
        path.toPolygonEdges -> SimplePolygon2D(path.toPolygonPathNodes.map(tiling.coords).asJava).centroid
      ).toMap

    private def innerEdges: List[Edge] =
      tiling.graphEdges.diff(tiling.perimeter.toRingEdges.toList)

    private def inversionSegments: List[LineSegment2D] =
      innerEdges
        .map(edge => polygonsCentreCoords.filter((edges, _) => edges.contains(edge)).values)
        .map(_.toList.toCouple)
        .map(LineSegment2D(_, _))

    private def inversionSVG(showInversion: Boolean): Option[Elem] =
      if showInversion then
        Option(
          group(
            Option(Title("Inversion")),
            Option(Description("Inverted tessellation by joining the centres of each two polygons sharing an edge")),
            inversionSegments.map(line)*
          ).withStyle(inversionStyle)
        )
      else
        None

    /** `group` with all the tiling features
     *
     * @param showPerimeter if true, the perimeter is shown
     * @param fillPolygons  if false, the polygons have no color fill
     * @param labelledNodes strategy for labelling nodes
     * @param markStyle     strategy for marking nodes
     * @param showGrowth    if true, growth by ordinal nodes is shown
     * @param showInversion if true, the inverted tessellation is shown
     */
    def tessellationGroup(showPerimeter: Boolean = true,
                          fillPolygons: Boolean = false,
                          labelledNodes: LabelledNodes = PERIMETER_ONLY,
                          markStyle: MarkStyle = MarkStyle.NONE,
                          showGrowth: Boolean = false,
                          showInversion: Boolean = false): Elem =
      group(
        Option(Title("Tiling")),
        Option(Description("Finite tessellation of regular polygons")),
        List(
          graphSVG,
          polygonsSVG(fillPolygons),
          inversionSVG(showInversion),
          perimeterSVG(showPerimeter),
          growthSVG(showGrowth),
          marksSVG(markStyle),
          labelsSVG(labelledNodes)
        ).getDefined *
      )

    /** `svg` element with metadata and `viewBox` fitting the tiling */
    def toViewBox(elems: Elem*): Elem =
      svg(tiling.toBox, elems :+ signatureMetadata *).addAttributes(rdfAttributes *)

    /** `svg` with all the tilings features
     *
     * @param showPerimeter if true, the perimeter is shown
     * @param fillPolygons  if false, the polygons have no color fill
     * @param labelledNodes strategy for labelling nodes
     * @param markStyle     strategy for marking nodes
     * @param showGrowth    if true, growth by ordinal nodes is shown
     * @param showInversion if true, the inverted tessellation is shown
     */
    def toSVG(showPerimeter: Boolean = true,
              fillPolygons: Boolean = false,
              labelledNodes: LabelledNodes = PERIMETER_ONLY,
              markStyle: MarkStyle = NONE,
              showGrowth: Boolean = false,
              showInversion: Boolean = false): Elem =
      toViewBox(
        tessellationGroup(showPerimeter, fillPolygons, labelledNodes, markStyle, showGrowth, showInversion)
      )
