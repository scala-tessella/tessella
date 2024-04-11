package io.github.scala_tessella.tessella
package conversion

import scala.xml.{Elem, Null, UnprefixedAttribute}

/** Methods for dealing with generic name-value pair attributes */
object NameValue:

  /** Name of a generic attribute */
  opaque type Name = String

  /** Companion object for [[Name]] */
  object Name:

    /** Create a [[Name]] from a `String` */
    def apply(s: String): Name =
      s

  /** Value of a generic attribute */
  opaque type Value = String

  /** Companion object for [[Value]] */
  object Value:

    /** Create a [[Value]] from a `String` */
    def apply(s: String): Value =
      s

  /** Formats string with name-value pair and given separator in between
   *
   * @param attribute name value-pair
   * @param separator a separator `Char`
   * @example {{{format(Name("a"), Value(1))(':') // "a:1"}}}
   */
  def format(attribute: (Name, Value))(separator: Char): String =
    attribute match
      case (name, value) => s"$name$separator$value"

  /** Formats string with given separator in between each element
   *
   * @param attributes list of elements
   * @param separator a separator `Char`
   * @example {{{format(List("a:1","b:2")(';') // "a:1;b:2"}}}
   */
  def format[T](attributes: List[T])(separator: Char): String =
    attributes.mkString(separator.toString)
