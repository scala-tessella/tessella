package io.github.scala_tessella.tessella

import RegularPolygon.{Polygon, PolygonOrdering}
import TilingUniformity.*
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

  /** Different polygons in the pattern */
  private def distinctPolygons: List[Polygon] =
    vertices.flatMap(_.vertex.toPolygons).distinct

  /** Number of different polygons in the pattern */
  def hedrality: Int =
    distinctPolygons.size

  /** Number of different full vertices in the pattern */
  def gonality: Int =
    distinctVertices.size

  /** Number of full vertices in the pattern */
  def uniformity: Int =
    vertices.size

  /** Checks whether the given tiling has the same polygons of the pattern */
  def hasSamePolygonsOf(tiling: Tiling): Boolean =
    distinctPolygons.sorted(PolygonOrdering) == tiling.distinctPolygons.sorted(PolygonOrdering)

  /** Checks whether the polygons of the given tiling are all contained in the pattern */
  def containsAllPolygonsOf(tiling: Tiling): Boolean =
    tiling.distinctPolygons.diff(distinctPolygons).isEmpty

  /** Checks whether the given tiling has the same full vertex configurations of the pattern */
  def hasSameFullVerticesOf(tiling: Tiling): Boolean =
    hasSamePolygonsOf(tiling) &&
      distinctVertices.sorted == tiling.distinctVertices.sorted

  /** Checks whether the full vertex configurations of the given tiling are all contained in the pattern */
  def containsAllFullVerticesOf(tiling: Tiling): Boolean =
    containsAllPolygonsOf(tiling) &&
      tiling.distinctVertices.diff(distinctVertices).isEmpty

  /** Checks whether the given tiling has the same symmetry classes of the pattern */
  def hasSameSymmetryClassesOf(tiling: Tiling): Boolean =
    hasSameFullVerticesOf(tiling) &&
      vertices.sorted == tiling.symmetryClasses.sorted

  /** Checks whether the symmetry classes of the given tiling are all contained in the pattern */
  def containsAllSymmetryClassesOf(tiling: Tiling): Boolean =
    containsAllFullVerticesOf(tiling) &&
      tiling.symmetryClasses.diff(vertices).isEmpty

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
