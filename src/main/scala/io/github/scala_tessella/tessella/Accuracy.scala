package io.github.scala_tessella.tessella

import scala.annotation.targetName

/** Provides comparison operations for a `Double` at variable precision. */
trait Accuracy:

  /** Standard multiplier to rounded `Long` */
  val ROUND_ACCURACY: Double = 1.0E6

  /** Standard precision for comparison, lower than [[http://geom-java.sourceforge.net/api/constant-values.html#math.geom2d.Shape2D.ACCURACY math.geom2d.Shape2D.ACCURACY]] */
  val LESSER_ACCURACY: Double = 1.0E-10

  extension (double: Double)

    /** Tests whether this `Double` is approximately equal to another, within a given accuracy.
     *
     * @param other     the compared `Double`
     * @param precision the given accuracy
     * @return true if double is approximately equal to ''other'',
     *         otherwise false.
     * @example {{{1.0 ~=(0.9, 0.1) // true}}}
     */
    @targetName("approxEqual")
    def ~=(other: Double, precision: Double = LESSER_ACCURACY): Boolean =
      (double - other).abs < precision

    /** Tests whether this `Double` is lesser than or approximately equal to another, within a given accuracy.
     *
     * @param other     the compared `Double`
     * @param precision the given accuracy
     * @return true if double is lesser than or approximately equal to ''other'',
     *         otherwise false.
     * @example {{{1.0 <|~=(0.9, 0.1) // true}}}
     */
    @targetName("ltOrApproxEqual")
    def <|~=(other: Double, precision: Double = LESSER_ACCURACY): Boolean =
      double < other || (double ~= (other, precision))

    /** Tests whether this `Double` is greater than or approximately equal to another, within a given accuracy.
     *
     * @param other     the compared `Double`
     * @param precision the given accuracy
     * @return true if double is greater than or approximately equal to ''other'',
     *         otherwise false.
     * @example {{{1.0 ~=|> (1.0999999, 0.1) // true}}}
     */
    @targetName("gtOrApproxEqual")
    def ~=|>(other: Double, precision: Double = LESSER_ACCURACY): Boolean =
      double > other || (double ~= (other, precision))

    /** Tests whether this `Double` is lesser than and not approximately equal to another, within a given accuracy.
     *
     * @param other     the compared `Double`
     * @param precision the given accuracy
     * @return true if double is lesser than and not approximately equal to ''other'',
     *         otherwise false.
     * @example {{{1.0 <&~!= (1.1, 0.1) // true}}}
     */
    @targetName("ltAndNotApproxEqual")
    def <&~!=(other: Double, precision: Double = LESSER_ACCURACY): Boolean =
      ! ~=|>(other, precision)

    /** Tests whether this `Double` is greater than and not approximately equal to another, within a given accuracy.
     *
     * @param other     the compared `Double`
     * @param precision the given accuracy
     * @return true if double is greater than and not approximately equal to ''other'',
     *         otherwise false.
     * @example {{{1.0 ~!=&> (0.8999999, 0.1) // true}}}
     */
    @targetName("gtAndNotApproxEqual")
    def ~!=&>(other: Double, precision: Double = LESSER_ACCURACY): Boolean =
      ! <|~=(other, precision)

    /** Convert this `Double` into a `Long` at a given accuracy, usually for sorting purposes.
     *
     * @param precision the given accuracy
     * @return a rounded Long.
     * @example {{{3.141592653589793.rounded() // 3141593}}}
     */
    def rounded(precision: Double = ROUND_ACCURACY): Long =
      Math.round(double * precision)
