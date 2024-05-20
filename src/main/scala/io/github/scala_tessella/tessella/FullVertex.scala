package io.github.scala_tessella.tessella

import Geometry.Radian
import RegularPolygon.{Polygon, Vertex}
import utility.Utils.invert
import utility.UtilsOption.sequence

import scala.annotation.targetName
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Try

/** The sequence of adjacent regular polygons forming a full circle around a vertex
 *
 * @param vertex a [[RegularPolygon.Vertex]]
 * @note being `private` a [[FullVertex]] cannot be created outside the class, thus ensuring vertex validation
 */
case class FullVertex private(vertex: Vertex) extends Ordered[FullVertex]:

  override def equals(any: Any): Boolean =
    any match
      case that: FullVertex => this.vertex.isSameFull(that.vertex)
      case _                => false

  def compare(that: FullVertex): Int =
    this.vertex.toPolygons.map(_.toSides) compare that.vertex.toPolygons.map(_.toSides)

  /** Minor version */
  def minor: FullVertex =
    FullVertex.maybe(Vertex.fullToMinor(vertex.toPolygons)).toOption.get

  override def toString: String =
    FullVertex.stringFrom(minor.vertex.toPolygons.map(_.toString) :+ "?")

  /** Compilable output */
  def toCompilableString: String =
    s"""FullVertex.s("$toString")"""

/** Companion object for [[FullVertex]] */
object FullVertex:

  private val subs: Map[Int, Char] =
    Map(
      2 -> '₂',
      3 -> '₃',
      4 -> '₄',
      5 -> '₅',
      6 -> '₆'
    )

  private lazy val supsInverted: Map[Char, Int] =
    subs.invert

  private def maybePolygonFromString(s: String): Option[Polygon] =
    Try(s.trim.toInt).toOption.flatMap(Vertex.maybeTessellablePolygon)

  /** Tries to decompose a string between parenthesis
   *
   * @param string checked string
   * @param open   opening parenthesis
   * @param close  closing parenthesis
   */
  def checkParenthesis(string: String, open: Char, close: Char): Either[String, Vector[Char]] =
    string.toList match
      case o :: t if o == open =>
        val tailChars: Vector[Char] =
          t.toVector
        if tailChars.isEmpty || tailChars.lastOption.getOrElse(close) != close then
          Left(s"Must end with $close")
        else
          Right(tailChars.init)
      case _ => Left(s"Must start with $open")

  private def maybeFromStrings(string: String): Either[String, Vector[Polygon]] =
    checkParenthesis(string, '(', ')').flatMap({
      case Vector() => Right(Vector())
      case centre =>
        val splits: Array[String] =
          centre.mkString
            .split('.')
            .flatMap(polygonsString =>
              polygonsString.indexOf('*') match
                case -1 =>
                  FullVertex.supsInverted.get(polygonsString.last) match
                    case Some(sup) => Vector.fill(sup)(polygonsString.init)
                    case None      => Vector(polygonsString)
                case i => Vector.fill(polygonsString.drop(i + 1).toInt)(polygonsString.take(i))
            )
        splits.foldLeft(Vector(): Vector[Option[Polygon]])((acc, r) => acc :+ maybePolygonFromString(r)).sequence match
          case Some(polygons) => Right(polygons)
          case _              => Left("Unknown symbol")
    })

  /** Error message for not being full */
  def notFullErrMsg(alpha: Radian): String =
    s"Interior angle must be full circle, found: $alpha radians"

  private def maybeFromVertex(vertex: Vertex): Either[String, FullVertex] =
    if vertex.isFull then
      Right(FullVertex(vertex))
    else
      Left(notFullErrMsg(vertex.alpha))

  /** Tries to create a `FullVertex` with validation.
   *
   * @param polygons vector of polygons
   * @return Either the full vertex or a failure message
   */
  def maybe(polygons: Vector[Polygon]): Either[String, FullVertex] =
    Try(Vertex(polygons)).toOption match
      case Some(vertex) => maybeFromVertex(vertex)
      case _            => Left(notFullErrMsg(polygons.alphaSum))

  def maybe(polygons: Polygon*): Either[String, FullVertex] =
    maybe(polygons.toVector)

  def maybe(s: String): Either[String, FullVertex] =
    maybeFromStrings(s).flatMap(FullVertex.maybe)

  /** Creates a `FullVertex` from string
   *
   * @throws NoSuchElementException if invalid
   */
  def s(s: String): FullVertex =
    maybe(s).toOption.get

  /** Create vertex with polygons of given sides
   *
   * @param sides numbers of sides of each polygon
   * @return an `Either` with a `FullVertex` if valid, otherwise an error message
   */
  private def maybeFromSides(sides: Vector[Int]): Either[String, FullVertex] =
    val maybeRegularPolygons =
      sides.map(Vertex.maybeTessellablePolygon).sequence match
        case Some(regularPolygons) => Right(regularPolygons)
        case _                     => Left("Invalid number of sides")
    maybeRegularPolygons.flatMap(FullVertex.maybe)

  @targetName("maybe_int")
  def maybe(sides: Int*): Either[String, FullVertex] =
    maybeFromSides(sides.toVector)

  /** Creates a `FullVertex` from sequence of polygons' sides
   *
   * @throws NoSuchElementException if invalid
   */
  def p(sides: Int *): FullVertex =
    maybeFromSides(sides.toVector).toOption.get

  private def stringFrom(endNotStackable: Vector[String]): String =
    val (descriptions, _): (List[String], List[String]) =
      endNotStackable
        .foldLeft((List(): List[String], List(): List[String]))({
          case ((cumulativeDescriptions, stackable), d) =>
            if (stackable.isEmpty || d == stackable.headOption.getOrElse("")) (cumulativeDescriptions, d :: stackable)
            else
              (cumulativeDescriptions :+ (stackable match
                case single :: Nil => single
                case _             => s"${stackable.headOption.getOrElse("")}${FullVertex.subs(stackable.size)}"
              ), List(d))
      })
    s"(${descriptions.mkString(".")})"

  /** The full vertices able to form edge-to-edge 1-uniform, monohedral tilings */
  private val regularPatterns: List[FullVertex] =
    Vertex.monoHedrals.map(FullVertex(_))

  /** The full vertices able to form edge-to-edge 1-uniform, 2 or 3-hedral tilings */
  private val semiRegularPatterns: List[FullVertex] =
    List(
      "(3₄.6)",
      "(3₃.4₂)",
      "(3₂.4.3.4)",
      "(3.4.6.4)",
      "(3.6.3.6)",
      "(3.12₂)",
      "(4.6.12)",
      "(4.8₂)"
    ).map(s)

  /** The full vertices able to be part of edge-to-edge k-uniform tilings */
  private val otherRegularPatterns: List[FullVertex] =
    List(
      "(3₂.4.12)",
      "(3₂.6₂)",
      "(3.4.3.12)",
      "(3.4₂.6)"
    ).map(s)

  /** The full vertices not able to form tilings */
  private val notExtensiblePatterns: List[FullVertex] =
    List(
      "(3.7.42)",
      "(3.8.24)",
      "(3.9.18)",
      "(3.10.15)",
      "(4.5.20)",
      "(5₂.10)"
    ).map(s)

  /** All 21 different valid full vertices */
  lazy val allPatterns: List[FullVertex] =
    regularPatterns ++ semiRegularPatterns ++ otherRegularPatterns ++ notExtensiblePatterns

  /** All 14 full vertices that can create a tessellation with gonality >= 2 */
  val patternableWithGonality2orMore: List[FullVertex] =
    regularPatterns ++ semiRegularPatterns.init ++ otherRegularPatterns
