package io.github.scala_tessella.tessella
package conversion

import ConverterDOT.{Attribute, Style}
import NameValue.{Name, Value, format}

/** Generic methods for producing .DOT file
 *
 * @see https://en.wikipedia.org/wiki/DOT_(graph_description_language)
 */
trait ConverterDOT:

  /** `color` attribute */
  val color: String => Attribute =
    Attribute.create("color")

  /** `fontcolor` attribute */
  val fontcolor: String => Attribute =
    Attribute.create("fontcolor")

  /** Styled edge */
  def edge(node1: String, node2: String, style: Style = Style(Nil *)): String =
    s"$node1 -- $node2$style"

  /** Styled node */
  def node(n: String, style: Style = Style(Nil *)): String =
    s"$n$style"

  /** Graph representation */
  def graph(edges: List[(String, String, Style)], nodes: List[(String, Style)]): String =
    val edgesString: String =
      edges.map(edge).mkString("\n")
    val nodesString: String =
      if nodes.nonEmpty then s"\n${nodes.map(node).mkString("\n")}"
      else ""
    s"""graph{
       |$edgesString$nodesString
       |}""".stripMargin

/** Companion methods for producing .DOT file */
object ConverterDOT:

  /** A DOT attribute
   *
   * @param attribute name-value pair
   */
  class Attribute(val attribute: (Name, Value)) extends AnyVal :

    override def toString: String =
      format(attribute)('=')

  /** Companion methods for [[ConverterDOT.Attribute]] */
  object Attribute:

    def create(name: String)(value: String): Attribute =
      Attribute((Name(name), Value(value)))

  /** A DOT style defined by one or more attributes
   *
   * @param attributes name-value pairs
   */
  class Style(val attributes: Attribute *) extends AnyVal :

    override def toString: String =
      if attributes.isEmpty then ""
      else s" [${format(attributes.toList)(' ')}]"
