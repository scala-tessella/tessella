package io.github.scala_tessella.tessella.conversion

import io.github.scala_tessella.tessella.Geometry.Box
import SharedML.*

import scala.xml.Elem

/** Generic methods for producing .HTML file. */
trait ConverterHTML extends UtilsXML:

  /** scale multiplier for div width */
  val scale: Int =
    50

  /** `html` element */
  def html(elems: Elem *): Elem =
    <html>{ elems.toNodeBuffer }</html>

  /** `head` element */
  def head(elems: Elem*): Elem =
    <head>{ elems.toNodeBuffer }</head>

  /** `body` element */
  def body(elems: Elem*): Elem =
    <body>{ elems.toNodeBuffer }</body>

  /** `html` element with `head` and `body` children
   *
   * @param title given to `head`
   * @param elems placed in `body`
   */
  def htmlTitled(title: Title, elems: Elem*): Elem =
    html(List(head(title.toElem), body(elems *)) *)

  /** `div` element */
  def div(elems: Elem*): Elem =
    <div>{ elems.toNodeBuffer }</div>

  private def boxedWidth(box9D: Box): Style =
    val enlarged: Box =
      box9D.enlarge(0.5)
    val width: Int =
      enlarged.width.toInt * scale
    Style(Attribute.create("width")(s"${width}px"))

  /** styled `div` element with width to fit a given box
   *
   * @param box2D box area with width to fit
   * @param elems placed in `div`
   */
  def divBoxed(box9D: Box, elems: Elem*): Elem =
    div(elems *).withStyle(boxedWidth(box9D))

  /** `h1` element */
  def h1(s: String): Elem =
    <h1>{ s }</h1>

  /** `h2` element */
  def h2(s: String): Elem =
    <h2>{ s }</h2>

  /** `h3` element */
  def h3(s: String): Elem =
    <h3>{ s }</h3>

  /** `p` element */
  def p(s: String): Elem =
    <p>{ s }</p>
