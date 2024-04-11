package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from a triangular net with different uniformity values */
trait UniTriangle extends Reticulate:

  /** 2-uniform tessellation [(3⁶);(3⁴.6)] (t=5, e=7) */
  def twoUniform3(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 3 * j) % 13 == 0)

  /** 3-uniform tessellation [(3².6²);(3.6.3.6);(6³)] (t=2, e=4) */
  def threeUniformOneOneOne5(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 3 * j) % 7 == 0 || (i + 3 * j) % 7 == 2)

  /** 3-uniform tessellation [(3⁶);(3⁴.6);(3.6.3.6)] (t=5, e=6) */
  def threeUniformOneOneOne6(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 4 * j) % 8 == 0)

  /** 4-uniform tessellation [2x(3⁶);(3⁴.6);(3².6²)] */
  def fourUniformTwoOneOne8(width: Int, height: Int): Either[String, Tiling] =
    triangleRect(width, height)((i, j) => (i + 1 * j) % 8 == 0)
