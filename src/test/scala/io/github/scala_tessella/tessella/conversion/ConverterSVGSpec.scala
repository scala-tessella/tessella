package io.github.scala_tessella.tessella
package conversion

import GeometryBase.Box9D
import SharedML.addAttributes
import SVG.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.xml.Elem

class ConverterSVGSpec extends AnyFlatSpec with should.Matchers {

  val box9D: Box9D =
    Box9D(-2.0, 10.0, -2.0, 10.0)

  "An SVG element" can "be created" in {
    val e: Elem =
      svg(box9D)
    prettyPrinter.format(e) shouldBe
      """<svg viewBox="-125.0 -125.0 650.0 650.0" xmlns="http://www.w3.org/2000/svg"></svg>"""
  }

  it can "have attributes added" in {
    val e: Elem =
      svg(box9D).addAttributes(rdfAttributes *)
    prettyPrinter.format(e) shouldBe
      """<svg xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" viewBox="-125.0 -125.0 650.0 650.0" xmlns="http://www.w3.org/2000/svg"></svg>"""

  }

  "A rect element" can "be created from a Box2D" in {
    val e: Elem =
      rect(box9D)
    prettyPrinter.format(e) shouldBe
      """<rect width="600.0" height="600.0" x="-100.0" y="-100.0"></rect>"""
  }

}
