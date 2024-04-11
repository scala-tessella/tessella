package io.github.scala_tessella.tessella
package creation

/** Contains fast methods to create a [[Tiling]] from an hexagonal net with uniformity 5 */
trait Uni5Hex extends Reticulate:

  /** 5-uniform tessellation [2x(3⁶);(3⁴.6);(3².6²;(6³)] */
  def fiveUniformTwoOneOneOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 == 1
      else
        pos % 6 < 3
    })

  /** 5-uniform tessellation [(3⁶);(3⁴.6);(3².6²);2x(6³)] */
  def fiveUniformTwoOneOneOne2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 8 < 2)

  /** 5-uniform tessellation [(3⁶);(3⁴.6);2x(3².6²);(6³)] */
  def fiveUniformTwoOneOneOne3(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 2
    })

  /** 5-uniform tessellation [(3⁶);(3⁴.6);(3².6²);2x(6³)] */
  def fiveUniformTwoOneOneOne4(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 7 < 2)

  /** 5-uniform tessellation [(3⁶);(3⁴.6);(3².6²);2x(6³)] */
  def fiveUniformTwoOneOneOne5(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + j) % 4 == 0)

  /** 5-uniform tessellation [(3⁶);(3⁴.6);2x(3².6²);(6³)] */
  def fiveUniformTwoOneOneOne6(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 6 == 0 || pos % 6 == 2
    })

  /** 5-uniform tessellation [(3⁶);3x(3².6²);(6³)] */
  def fiveUniformThreeOneOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 8 == 0 || pos % 8 == 3
    })

  /** 5-uniform tessellation [(3⁶);3x(3².6²);(6³)] */
  def fiveUniformThreeOneOne2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 7 == 0 || pos % 7 == 3
    })

  /** 5-uniform tessellation [3x(3⁶);(3².6²);(6³)] */
  def fiveUniformThreeOneOne3(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 > 1)

  /** 5-uniform tessellation [3x(3⁶);(3².6²);(6³)] */
  def fiveUniformThreeOneOne4(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      if (j % 2 == 0)
        pos % 3 < 2
      else
        pos % 6 < 1 || pos % 6 > 3
    })

  /** 5-uniform tessellation [(3⁶);2x(3².6²);2x(6³)]
   *
   * @see http://probabilitysports.com/tilings.html?u=0&n=5&t=2
   */
  def fiveUniformTwoTwoOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 7 == (j * 3) % 7)

  /** 5-uniform tessellation [2x(3⁶);(3⁴.6);2x(3².6²)] */
  def fiveUniformTwoTwoOne2(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => i % 3 == j % 3 || (i % 3 == 0 && j % 3 == 1))

  /** 5-uniform tessellation [(3⁶);2x(3².6²);2x(6³)] */
  def fiveUniformTwoTwoOne3(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => (i + 2 * j) % 6 == 0)

  /** 5-uniform tessellation [2x(3⁶);2x(3⁴.6);(3².6²)] */
  def fiveUniformTwoTwoOne4(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)((i, j) => {
      val pos = i + 2 * j
      pos % 5 == 1 || pos % 5 > 2
    })

  /** 5-uniform tessellation [4x(3⁶);(3⁴.6)] */
  def fiveUniformFourOne(width: Int, height: Int): Either[String, Tiling] =
    hexagonRect(width, height)(_ % 3 != 0 || _ % 3 != 0)
