package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from an hexagonal net with uniformity 5 */
trait Uni5Hex extends Reticulate:

  /** 5-uniform tessellation [2x(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def pattern_2x333333_33336_3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 == 1
      else
        pos % 6 < 3
    })

  /** 5-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);2x(6₃)] */
  def pattern_333333_33336_3366_2x666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 8 < 2)
  
  /** 5-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);2x(6₃)] */
  def pattern_333333_33336_3366_2x666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 7 < 2)

  /** 5-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);2x(6₃)] */
  def pattern_333333_33336_3366_2x666_alt2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + j) % 4 == 0)
  
  /** 5-uniform tessellation [(3₆);(3₄.6);2x(3₂.6₂);(6₃)] */
  def pattern_333333_33336_2x3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 2
    })
  /** 5-uniform tessellation [(3₆);(3₄.6);2x(3₂.6₂);(6₃)] */
  def pattern_333333_33336_2x3366_666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 6 == 0 || pos % 6 == 2
    })

  /** 5-uniform tessellation [(3₆);3x(3₂.6₂);(6₃)] */
  def pattern_333333_3x3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 8 == 0 || pos % 8 == 3
    })

  /** 5-uniform tessellation [(3₆);3x(3₂.6₂);(6₃)] */
  def pattern_333333_3x3366_666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 3
    })

  /** 5-uniform tessellation [3x(3₆);(3₄.6);(3₂.6₂)] */
  def pattern_3x333333_33336_3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 > 1)

  /** 5-uniform tessellation [3x(3₆);(3₂.6₂);(6₃)] */
  def pattern_3x333333_33336_3366_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 < 1 || pos % 6 > 3
    })

  /** 5-uniform tessellation [(3₆);2x(3₂.6₂);2x(6₃)]
   *
   * @see http://probabilitysports.com/tilings.html?u=0&n=5&t=2
   */
  def pattern_333333_2x3366_2x666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 7 == (j * 3) % 7)

  /** 5-uniform tessellation [(3₆);2x(3₂.6₂);2x(6₃)] */
  def pattern_333333_2x3366_2x666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 == 0)

  /** 5-uniform tessellation [2x(3₆);(3₄.6);2x(3₂.6₂)] */
  def pattern_2x333333_33336_2x3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 3 == j % 3 || (i % 3 == 0 && j % 3 == 1))

  /** 5-uniform tessellation [2x(3₆);2x(3₄.6);(3₂.6₂)] */
  def pattern_2x333333_2x33336_3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 5 == 1 || pos % 5 > 2
    })

  /** 5-uniform tessellation [4x(3₆);(3₄.6)] */
  def pattern_4x333333_33336(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 != 0 || _ % 3 != 0)
