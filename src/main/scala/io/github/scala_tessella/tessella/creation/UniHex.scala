package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from an hexagonal net with different uniformity values */
trait UniHex extends Reticulate:

  /** 2-uniform tessellation [(3₆);(3₂.6₂)] (t=2, e=3) */
  def pattern_333333_3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 == _ % 3)

  /** 2-uniform tessellation [(3₆);(3₄.6)] (t=3, e=3)
   * 
   * @note obtainable also with a triangleNetVariant(_, _)(_ % 3 == 0 && _ % 3 == 0)
   */
  def pattern_333333_33336(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 != _ % 3)

  /** 3-uniform tessellation [(3₆);(3₂.6₂);(6₃)] (t=2, e=3) */
  def pattern_333333_3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 2 == 0 && _ % 2 == 0)

  /** 3-uniform tessellation [(3₆);(3₄.6);(3₂.6₂)] (t=5, e=8) */
  def pattern_333333_33336_3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 4 < 2)

  /** 3-uniform tessellation [(3₆);(3₄.6);(3₂.6₂)] (t=3, e=5) */
  def pattern_333333_33336_3366_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((_, j) => j % 2 == 0)

  /** 3-uniform tessellation [2x(3₆);(3₄.6)] (t=3, e=4) */
  def pattern_2x333333_33336(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 2 == 0 || _ % 2 == 0)

  /** 7-uniform tessellation [(3₆);2x(3₂.6₂);4x(6₃)] */
  def pattern_333333_2x3366_4x666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 10 == (j * 8) % 10)
