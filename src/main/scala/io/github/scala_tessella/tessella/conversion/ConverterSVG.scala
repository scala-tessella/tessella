package io.github.scala_tessella.tessella
package conversion

import ConverterSVG.Description
import Geometry.*
import SharedML.*

import scala.xml.{Elem, Null, UnprefixedAttribute}

/** Generic methods for producing .SVG file
 *
 * @see https://en.wikipedia.org/wiki/Scalable_Vector_Graphics
 */
trait ConverterSVG extends UtilsXML:

  /** scale multiplier for all elements */
  val scale: Double =
    50.0

  /** round accuracy for scaling */
  val svgAccuracy: Double =
    1000000.0

  private def rescale(double: Double): Double =
    Math.round(double * svgAccuracy * scale) / svgAccuracy

  extension (point: Point)

    private def scaled: (Double, Double) = (rescale(point.x), rescale(point.y))

  private def formatPoints(points: Iterable[Point]): String =
    points.map(_.scaled).map((x, y) => s"$x,$y").mkString(" ")

  extension (elem: Elem)

    /** add `points` attribute for `polyline` and `polygon` */
    private def withPoints(points: Iterable[Point]): Elem =
      elem.addAttributes(Attribute.create("points")(formatPoints(points)))

  /** `fill` attribute */
  val fill: String => Attribute =
    Attribute.create("fill")

  /** `stroke` attribute */
  val stroke: String => Attribute =
    Attribute.create("stroke")

  private def framedViewBox(box: Box): String =
    val enlarged: Box =
      box.enlarge(0.5)
    val newMin: Point =
      Point(enlarged.x0, enlarged.y0)
    val (minX, minY): (Double, Double) =
      newMin.scaled
    val (maxX, maxY): (Double, Double) =
      Point(enlarged.x1, enlarged.y1).minus(newMin).scaled
    s"$minX $minY $maxX $maxY"

  /** `svg` element with `viewBox` to fit a given box
   *
   * @param box   box area with width and height to fit
   * @param elems placed in `svg`
   */
  def svg(box: Box, elems: Elem *): Elem =
    <svg viewBox={ s"${framedViewBox(box)}" } xmlns="http://www.w3.org/2000/svg">{ elems.toNodeBuffer }</svg>

  /** `metadata` element */
  def metadata(elems: Elem *): Elem =
    <metadata>{ elems.toNodeBuffer }</metadata>

  /** `group` element with optional title and description
   *
   * @param title optional title
   * @param desc optional description
   * @param elems placed in `group`
   */
  def group(title: Option[Title],
            desc: Option[Description],
            elems: Elem *): Elem =
    val titleDesc: Seq[Elem] =
      Seq(title.map(_.toElem), desc.map(_.toElem)).filter(_.isDefined).map(_.get)
    <g>{ (titleDesc ++ elems).toNodeBuffer }</g>

  /** `text` element
   *
   * @param point spatial coordinates
   * @param s     text
   */
  def text(point: Point, s: String): Elem =
    val (x, y) = point.scaled
    <text x={ s"$x" } y={ s"$y" } >{ s }</text>

  /** `rect` element
   *
   * @param box spatial coordinates
   */
  def rect(box: Box): Elem =
    val methods: List[Box => Double] =
      List(_.width, _.height, _.x0, _.y0)
    (methods.map(_.apply(box)).map(rescale): @unchecked) match
      case width :: height :: x :: y :: Nil =>
        <rect width={ s"$width" } height={ s"$height" } x={ s"$x" } y={ s"$y" }></rect>

  /** `polygon` element
   *
   * @param points spatial coordinates
   * @param elems placed in `polygon`
   */
  def polygon(points: Iterable[Point], elems: Elem *): Elem =
    (if elems.isEmpty then
      <polygon/>
    else
      <polygon>{ elems.toNodeBuffer }</polygon>
    ).withPoints(points)

  /** `polygon` element from `SimplePolygon` */
  def polygon(simplePolygon: SimplePolygon): Elem =
    polygon(simplePolygon.getVertices)

  /** `polyline` element
   *
   * @param points spatial coordinates
   * @param elems placed in `polygon`
   */
  def polyline(points: Iterable[Point], elems: Elem *): Elem =
    (if elems.isEmpty then
      <polyline/>
    else
      <polyline>{ elems.toNodeBuffer }</polyline>
    ).withPoints(points)

  /** `line` element
   *
   * @param point1 spatial coordinates of one endpoint
   * @param point2 spatial coordinates of other endpoint
   */
  def line(point1: Point, point2: Point): Elem =
    val (x1, y1): (Double, Double) =
      point1.scaled
    val (x2, y2): (Double, Double) =
      point2.scaled
    <line x1={ s"$x1" } y1={ s"$y1" } x2={ s"$x2" } y2={ s"$y2" } />

  /** `line` element from `LineSegment` */
  def line(segment: LineSegment): Elem =
    line(segment.point1, segment.point2)

  /** `circle` element
   *
   * @param center spatial coordinates
   * @param radius length
   */
  def circle(center: Point, radius: Double): Elem =
    val (cx, cy): (Double, Double) =
      center.scaled
    <circle cx={ s"$cx" } cy={ s"$cy" } r={ s"${rescale(radius)}"} />

  /** `animate` element with attributes */
  def animate(attributes: Attribute *): Elem =
    val element: Elem =
      <animate />
    element.addAttributes(attributes *)

  private def pointsAnimation(from: Iterable[Point],
                              to: Iterable[Point],
                              values: List[Iterable[Point]]): Elem =
    animate(List(
      Attribute.create("attributeName")("points"),
      Attribute.create("dur")("5s"),
      Attribute.create("from")(formatPoints(from)),
      Attribute.create("to")(formatPoints(to)),
      Attribute.create("values")(values.map(formatPoints).mkString(";"))
    ) *)

  /** Animated polygon
   *
   * @param pointsSeries spatial coordinates
   */
  def animatedPolygon(pointsSeries: List[Iterable[Point]]): Elem =
    val animation: Elem =
      pointsAnimation(
        pointsSeries.head,
        pointsSeries.last,
        pointsSeries.tail.init
      )
    polygon(pointsSeries.last, animation)

  /** Animated polyline developing sequentially from start to end
   *
   * @param points spatial coordinates
   */
  def animatedPolyline(points: Iterable[Point]): Elem =
    val scanned: Iterable[List[Point]] =
      points.tail.scanLeft(List(points.head))(_ ++ List(_))
    val animation: Elem =
      pointsAnimation(
        scanned.head,
        scanned.last,
        scanned.tail.init.toList
      )
    polyline(points, animation)

  private def arrowHeadPoints(tip: Point, origin: Point): List[Point] =
    val angle: Radian =
      LineSegment(tip, origin).horizontalAngle
    val delta: Double =
      0.5

    def vertex(upper: Boolean): Point =
      val variation: Radian =
        Radian(if upper then delta else -delta)
      tip.plus(Point.createPolar(0.1, angle + variation))

    List(tip, vertex(true), vertex(false))

  /** Arrow head as a triangle
   *
   * @param tip spatial coordinates of endpoint with arrow
   * @param origin spatial coordinates of other endpoint
   */
  def arrowHead(tip: Point, origin: Point, elems: Elem *): Elem =
    polygon(arrowHeadPoints(tip, origin), elems *)

  /** Animated arrow head
   *
   * @param points spatial coordinates of the arrow movements
   */
  def animatedArrowHead(points: Iterable[Point]): Elem =
    val listed = points.toList
    val slided: List[List[Point]] = (listed.head :: listed).sliding(2).toList
    val animation: Elem =
      pointsAnimation(
        arrowHeadPoints(slided.head.last, slided.head.head),
        arrowHeadPoints(slided.last.last, slided.last.head),
        slided.tail.init.map(ps => arrowHeadPoints(ps.last, ps.head))
      )
    arrowHead(slided.last.last, slided.last.head, animation)

  /** Animated arrow and polyline
   *
   * @param points spatial coordinates of the polyline
   * @param arrowHeadStyle arrow head style
   * @param polylineStyle polyline style
   */
  def animatedPolylineArrow(points: Iterable[Point],
                            arrowHeadStyle: Style = Style(Nil *),
                            polylineStyle: Style = Style(Nil *)): Elem =
    group(None, None, elems = List(
      animatedPolyline(points).withStyle(polylineStyle),
      animatedArrowHead(points).withStyle(arrowHeadStyle)
    ) *)

/** Companion methods for producing .SVG file */
object ConverterSVG:

  /** A description `desc` element
   *
   * @param underlying `String`
   */
  class Description(val underlying: String) extends AnyVal:

    def toElem: Elem =
      <desc>{underlying}</desc>
