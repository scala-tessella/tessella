package io.github.scala_tessella.tessella.conversion

import NameValue.*

import scala.xml.{Elem, Null, UnprefixedAttribute}

/** Shared methods for XML or HTML conversion. */
object SharedML:

  /** An XML or HTML title tag.
   *
   * @param underlying `String`
   */
  class Title(val underlying: String) extends AnyVal:

    def toElem: Elem = <title>{ underlying }</title>

  /** An XML or HTML attribute.
   *
   * @param attribute name-value pair
   */
  class Attribute(val attribute: (Name, Value)) extends AnyVal :

    override def toString: String =
      format(attribute)(':')

  /** Companion methods for [[SharedML.Attribute]]. */
  object Attribute:

    /** Creates an attribute with currying */
    def create(name: String)(value: String): Attribute =
      Attribute((Name(name), Value(value)))

  /** An XML or HTML style defined by one or more attributes.
   *
   * @param attributes name-value pairs
   */
  class Style(val attributes: Attribute*) extends AnyVal :

    override def toString: String =
      format(attributes.toList)(';')

  extension (elem: Elem)

    /** Adds attributes to an element */
    def addAttributes(attributes: Attribute*): Elem =
      attributes.reverse.foldLeft(elem)((element, attribute) => attribute.attribute match
        case (name, value) => element % UnprefixedAttribute(name.toString, value.toString, Null)
      )

    /** Adds style attribute to an element */
    def withStyle(style: Style): Elem =
      if style.attributes.isEmpty then elem
      else elem.addAttributes(Attribute.create("style")(style.toString))
