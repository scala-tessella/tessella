package io.github.scala_tessella.tessella
package creation

import TilingGrowth.{FullStrategy, OtherNodeStrategy, PerimeterStrategy, growFullVertex}
import TilingGrowth.OtherNodeStrategy.*
import TilingGrowth.PerimeterStrategy.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*

/** Contains slow methods to create a [[Tiling]] with growth governed by quadratic functions */
trait Quadratic:

  /**
   * (a, b, c) elements of the polynomial ax&#94;2+bx+c)
   */
  private type Terms = (Double, Double, Double)

  /** Function to calculate the polynomial of grade 2 with the given terms
   *
   * @param terms elements of the polynomial
   */
  private def quadratic(terms: Terms): Double => Double =
    val (a, b, c): Terms = terms
    x => a * x * x + b * x + c

  private def fromTerms(terms: Terms, side: Int): Int =
    quadratic(terms)(side.toDouble).toInt

  private def fullOfSide(fullVertex: FullVertex, side: Int, f: Int => Int, strategy: FullStrategy): Either[String, Tiling] =
    for
      _           <- side.refineEither[Positive DescribedAs "Side should be greater than 0"]
      maybeTiling <- growFullVertex(f(side), fullVertex, strategy)
    yield
      maybeTiling

  val standardStrategy: FullStrategy =
    (List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL), true)

  val standardStrategyFromSmaller: FullStrategy =
    standardStrategy.copy(_3 = false)

  /** Grows a (3*6) hexagon of given side
   *
   * @note the size in polygons is 6x&#94;2 where x = side
   * @param side length in units
   */
  def triangularHex(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(3⁶)"), side, (i: Int) => fromTerms((6, -12, 6), i + 1), standardStrategy)

  /** Grows a (4*4) square of given side
   *
   * @note the size in polygons is x&#94;2 where x = side
   * @param side length in units
   */
  def squareNet(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(4⁴)"), side, fromTerms((1, 0, 0), _), standardStrategy)

  /** Grows a (6*3) hexoid of given side
   *
   * @note the size in polygons is 3x&#94;2-3x+1 where x = side
   * @param side length in hexagons (approx. √3 each)
   */
  def hexagonalHexoid(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(6³)"), side, fromTerms((3, -3, 1), _), standardStrategy)

  /** Grows a (3*4.6) hexoid of given side
   *
   * @note https://www.wolframalpha.com/input?i=quadratic+fit+%7B1%2C+19%2C+37%2C+61%2C+103%2C+139%2C+193%2C+247%2C+319%2C+391%2C+469%2C+565%7D
   * @param side length is ???
   */
  def triHexagonalHexoid(side: Int): Either[String, Tiling] =
    val strategy: FullStrategy =
      (List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(NARROWER_ANGLE, LOWER_ORDINAL), false)
    val sizes: List[Int] =
      List(1, 19, 37, 61, 103, 139, 193, 247, 319, 391, 469, 565)
    fullOfSide(FullVertex.s("(3⁴.6)"), side, (i: Int) => sizes(i - 1), strategy)

  /** Grows a (3*3.4*2) squaroid of given side
   *
   * @note the size in polygons is 21x&#94;2-27x+8 where x = side
   * @note https://www.wolframalpha.com/input/?i=quadratic+fit+%7B18%2C+78%2C+180%7D
   * @param side length is ???
   */
  def triSquareSquaroid(side: Int): Either[String, Tiling] =
    val strategy: FullStrategy =
      (List(LOWEST_ORDINAL), List(BEFORE_PERIMETER), true)
    fullOfSide(FullVertex.s("(3³.4²)"), side, fromTerms((21, -27, 8), _), strategy)

  /** Grows a (3.4.6.4) hexoid of given side
   *
   * @note the size in polygons is 18x&#94;2-30x+13 where x = side
   * @param side length in hexagons (approx. ? each)
   */
  def triSquareHexagonalHexoid(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(3.4.6.4)"), side, fromTerms((18, -30, 13), _), standardStrategy)

  /** Grows a (3.6.3.6) hexagon of given side
   *
   * @note the size in polygons is 9x&#94;2-9x+1 where x = side
   * @param side length in hexagons (2x-1 units)
   */
  def triHexagonalHex(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(3.6.3.6)"), side, fromTerms((9, -9, 1), _), standardStrategy)

  /** Grows a (3.12*2) hexoid of given side
   *
   * @note the size in polygons is 9x&#94;2-9x+1 where x = side
   * @param side length in dodecagons (approx. ???)
   */
  def triDodecagonalHexoid(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(3.12²)"), side, fromTerms((9, -9, 1), _), standardStrategy)

  /** Grows a (3².4.3.4) squaroid of given side
   *
   * @note the size in polygons is 12x&#94;2+16x+1 where x = side
   * @param side length in
   */
  def triSquareSquaroid2(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(3².4.3.4)"), side, fromTerms((12, 16, 1), _), standardStrategyFromSmaller)

  /** Grows a (4.6.12) hexoid of given side
   *
   * @note the size in polygons is 18x&#94;2-6x+1 where x = side
   * @param side length in dodecagons (approx. ???)
   */
  def squareHexDodecagonalHexoid(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(4.6.12)"), side, fromTerms((18, -6, 1), _), standardStrategy)

  /** Grows a (4.8*2) squaroid of given side
   *
   * @note the size in polygons is 4x&#94;2-4x+1 where x = side
   * @param side length in squares (approx. ? each)
   */
  def squareOctogonalSquaroid(side: Int): Either[String, Tiling] =
    fullOfSide(FullVertex.s("(4.8²)"), side, fromTerms((4, -4, 1), _), standardStrategyFromSmaller)
