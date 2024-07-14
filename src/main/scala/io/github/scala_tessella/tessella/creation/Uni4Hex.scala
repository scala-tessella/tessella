package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from an hexagonal net with uniformity 4 */
trait Uni4Hex extends Reticulate:

  /** 4-archimedean tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def pattern_333333_33336_3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 < 2)

  /** 4-archimedean tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def pattern_333333_33336_3366_666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 5 < 2)

  /** 4-archimedean tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def pattern_333333_33336_3366_666_alt2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((_, j) => j % 3 == 0)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def pattern_333333_3366_2x666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 7 == (j * 4) % 7)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)]
   *
   * @see http://probabilitysports.com/tilings.html?u=0&n=4&t=1
   */
  def pattern_333333_3366_2x666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => j % 2 == 0 && i % 6 == (j * 4) % 6)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def pattern_333333_3366_2x666_alt2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => ((i + 2 * j) % 6 == 0 || (i + 2 * j) % 6 == 2) && j % 2 == 0)

  /** 4-uniform tessellation [(3₆);2x(3₂.6₂);(6₃)] */
  def pattern_333333_2x3366_666(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 3 == 0 && j % 3 < 2)

  /** 4-uniform tessellation [(3₆);2x(3₂.6₂);(6₃)] */
  def pattern_333333_2x3366_666_alt(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      // alternate rows between 3 and 6
      val step = (j % 2 + 1) * 3
      (i + 2 * j) % step == 0
    })

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def pattern_333333_3366_2x666_alt3(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 == 0 && _ % 3 == 0)

  /** 4-uniform tessellation [(3₆);2x(3₄.6);(3₂.6₂)] */
  def pattern_333333_2x33336_3366(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 == 0 || pos % 6 == 4
    })
