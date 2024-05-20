package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from an hexagonal net with uniformity 4 */
trait Uni4Hex extends Reticulate:

  /** 4-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def fourUniformOneOneOneOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 < 2)

  /** 4-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def fourUniformOneOneOneOne1(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 5 < 2)

  /** 4-uniform tessellation [(3₆);(3₄.6);(3₂.6₂);(6₃)] */
  def fourUniformOneOneOneOne2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((_, j) => j % 3 == 0)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def fourUniformTwoOneOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 7 == (j * 4) % 7)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)]
   *
   * @see http://probabilitysports.com/tilings.html?u=0&n=4&t=1
   */
  def fourUniformTwoOneOne2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => j % 2 == 0 && i % 6 == (j * 4) % 6)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def fourUniformTwoOneOne3(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => ((i + 2 * j) % 6 == 0 || (i + 2 * j) % 6 == 2) && j % 2 == 0)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def fourUniformTwoOneOne4(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 3 == 0 && j % 3 < 2)

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def fourUniformTwoOneOne5(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      // alternate rows between 3 and 6
      val step = (j % 2 + 1) * 3
      (i + 2 * j) % step == 0
    })

  /** 4-uniform tessellation [(3₆);(3₂.6₂);2x(6₃)] */
  def fourUniformTwoOneOne6(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 == 0 && _ % 3 == 0)

  /** 4-uniform tessellation [(3₆);2x(3₄.6);(3₂.6₂)] */
  def fourUniformTwoOneOne7(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 == 0 || pos % 6 == 4
    })
