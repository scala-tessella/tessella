package io.github.scala_tessella.tessella

import utility.Utils.*
import utility.UtilsOption.sequence

import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Try

/** Signature pattern of an uniform infinite  tessellation of unit regular polygons */
case class Pattern(vertices: List[FullVertex]) extends Ordered[Pattern]:

  override def toString: String =
    def prefix(size: Int) = size match
      case 1 => ""
      case n => s"${n}x"

    val grouped: Map[FullVertex, List[FullVertex]] =
      vertices.groupBy(identity)
    s"[${grouped.keys.toList.sorted.map(vertex => s"${prefix(grouped(vertex).size)}${vertex.toString}").mkString(";")}]"

  /** Compilable output */
  def toCompilableString: String =
    s"""Pattern.s("$toString")"""

  def compare(that: Pattern): Int =
    this.vertices compare that.vertices

  /** Filters out duplicates */
  lazy val distinctVertices: List[FullVertex] =
    vertices.distinct

  /** Number of different polygons in the pattern */
  def hedrality: Int =
    vertices.flatMap(_.vertex.toPolygons).distinct.size

  /** Number of different full vertices in the pattern */
  def gonality: Int =
    distinctVertices.size

  /** Number of full vertices in the pattern */
  def uniformity: Int =
    vertices.size

/** Companion object for [[Pattern]] */
object Pattern:

  /** Creates a `Pattern`
   *
   * @note two different infinite tessellations can have the same pattern, is not an unique identifier
   * @param vertices a list of [[FullVertex]] each representing a different uniformity group
   */
  def apply(vertices: List[FullVertex]): Pattern =
    new Pattern(vertices.map(_.minor).sorted)

  def apply(vertices: FullVertex *): Pattern =
    Pattern(vertices.toList)

  /** Tries to create a `Pattern` with validation.
   *
   * @param s string
   * @return Either the pattern or a failure message
   */
  def maybe(s: String): Either[String, Pattern] =
    FullVertex.checkParenthesis(s, '[', ']').flatMap(
      _.mkString.split(";").toVector.flatMap(elem => elem.toList match
        case h :: _ if h == '(' => Vector(FullVertex.maybe(elem).toOption)
        case _                  => elem.split("x").toVector.toCouple match
          case (times, full) => Try(times.toInt).toOption match
            case Some(_) => Vector.fill(times.toInt)(FullVertex.maybe(full).toOption)
            case None    => Vector(None)
      ).sequence match
        case Some(vertices) => Right(Pattern(vertices.toList))
        case None           => Left("malformed pattern")
    )

  /** Creates a `Pattern` from string
   *
   * @throws NoSuchElementException if invalid
   */
  def s(s: String): Pattern =
    maybe(s).toOption.get
