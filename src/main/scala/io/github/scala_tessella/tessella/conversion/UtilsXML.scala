package io.github.scala_tessella.tessella.conversion

import scala.xml.*

/** Useful additional methods for XML. */
trait UtilsXML:

  extension (elems: Iterable[Elem])

    /** Creates a `NodeBuffer` of elements */
    def toNodeBuffer: NodeBuffer =
      elems.foldLeft(NodeBuffer())(_ &+ _)

  /** With set width and indentation */
  val prettyPrinter: PrettyPrinter =
    PrettyPrinter(2000, 2)
