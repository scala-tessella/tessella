package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from a triangular net with different uniformity values */
trait UniTriangle extends Reticulate:

  /** 2-uniform tessellation [(3₆);(3₄.6)] (t=5, e=7) */
  def pattern_333333_33336_alt(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 3 * j) % 13 == 0)

  /** 3-uniform tessellation [(3₂.6₂);(3.6.3.6);(6₃)] (t=2, e=4) */
  def pattern_3366_3636_666(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 3 * j) % 7 == 0 || (i + 3 * j) % 7 == 2)

  /** 3-uniform tessellation [(3₆);(3₄.6);(3.6.3.6)] (t=5, e=6) */
  def pattern_333333_33336_3636(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 4 * j) % 8 == 0)

  /** 4-uniform tessellation [2x(3₆);(3₄.6);(3₂.6₂)] */
  def pattern_2x333333_33336_3366(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 1 * j) % 8 == 0)
