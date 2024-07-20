package io.github.scala_tessella.tessella
package creation

import Topology.{Edge, Node, compact, withOnlyNodes}
import utility.Utils.toCouple

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.scala_tessella.ring_seq.RingSeq.{slidingO, rotateLeft}

type EvenAndGreater[A] = Greater[A] & Multiple[2]

/** Contains fast methods to create a [[Tiling]] as a reticulate of arbitrary size */
trait Reticulate:

  private def squareNetEdges(x: Int, y: Int): List[Edge] =
    val horiz: IndexedSeq[(Node, Node)] =
      for
        i <- 1 to x
        j <- 0 to y
        h = i + (x + 1) * j
      yield (Node(h), Node(h + 1))
    val vert: IndexedSeq[(Node, Node)] =
      for
        i <- 1 to (x + 1)
        j <- 0 until y
      yield (Node(i + (x + 1) * j), Node(i + (x + 1) * (j + 1)))
    (horiz ++ vert).toList.map(Edge(_))

  private def triangleNetEdges(x: Int, y: Int): List[Edge] =
    val h: Int =
      x / 2
    val diag: IndexedSeq[(Node, Node)] =
      for
        i <- 1 to h
        j <- 0 until y
      yield (Node(i + (h + 1) * j), Node(i + 1 + (h + 1) * (j + 1)))
    squareNetEdges(h, y) ++ diag.map(Edge(_)).toList

  private def triangleTriangleEdges(sides: Int): List[Edge] =
    val goodHalf: IndexedSeq[Node] =
      for
        i <- 1 to sides + 1
        j <- i to (sides + 1) * 2
        h = i + (sides + 2) * j
      yield Node(h)
    triangleNetEdges((sides + 1) * 2, sides + 1).withOnlyNodes(goodHalf.toList).compact

  private def hexagonNetEdges(x: Int, y: Int): List[Edge] =
    val horiz: IndexedSeq[(Node, Node)] =
      for
        i <- 0 to x * 2
        j <- 0 to y
        h = i + (x + 1) * j * 2
      yield (Node(h), Node(h + 1))
    val vert: IndexedSeq[(Node, Node)] =
      for
        i <- 1 to (x + 1)
        j <- 0 until y
        v = (x + 1) * 2; w = i * 2 - 1
      yield (Node(w + v * j), Node(w - 1 + v * (j + 1)))
    (horiz.tail.init ++ vert).toList.map(Edge(_))

  private def hexTrianguloidEdges(sides: Int): List[Edge] =
    val goodHalf: IndexedSeq[Node] =
      (
        for
          j <- 0 to sides
          i <- 0 to (sides - j + 1) * 2
          h = i + (sides + 1) * j * 2
        yield Node(h)
      ).tail
    hexagonNetEdges(sides, sides).withOnlyNodes(goodHalf.toList).compact

  private def octagonNetEdges(x: Int, y: Int): List[Edge] =
    val couples: IndexedSeq[(Node, Node)] =
      (for
        i <- 0 to x
        j <- 0 to y
        h = (i + (x + 1) * j) * 4 + 1
        square = (h to h + 3).toList.slidingO(2).toList.map(_.map(Node(_))).map(_.toCouple)
        connectorX = if i == x then Nil else List((Node(h + 2), Node(h + 4)))
        connectorY = if j == y then Nil else List((Node(h + 3), Node(h + 1 + (x + 1) * 4)))
      yield square ++ connectorX ++ connectorY)
        .flatten
    couples.toList.map(Edge(_))

  private type IntRef = Int => Either[String, Int]

  private def triangular(side: Int, rs: IntRef, f: Int => List[Edge]): Either[String, Tiling] =
    for
      s <- rs(side)
      t <- Tiling.maybe(f(s))
    yield
      t

  private def rectangular(width: Int, height: Int, rx: IntRef, ry: IntRef, f: (Int, Int) => List[Edge]): Either[String, Tiling] =
    for
      x <- rx(width)
      y <- ry(height)
      t <- Tiling.maybe(f(x, y))
    yield
      t

  private val width_even_gt_zero: IntRef =
    _.refineEither[EvenAndGreater[0] DescribedAs "Width should be even and greater than 0"]

  private val width_even_gt_two: IntRef =
    _.refineEither[EvenAndGreater[2] DescribedAs "Width should be even and greater than 2"]

  val width_gt_zero: IntRef =
    _.refineEither[Positive DescribedAs "Width should be greater than 0"]

  val height_gt_zero: IntRef =
    _.refineEither[Positive DescribedAs "Height should be greater than 0"]

  private val height_gt_one: IntRef =
    _.refineEither[Greater[1] DescribedAs "Height should be greater than 1"]

  private val side_gt_zero: IntRef =
    _.refineEither[Positive DescribedAs "Side should be greater than 0"]

  /** Creates a rectangular reticulate of width by height triangles
   *
   * @param width size must be even and greater than 0
   * @param height size must be greater then 0
   */
  def pattern_333333(width: Int, height: Int): Either[String, Tiling] =
    rectangular(width, height, width_even_gt_zero, height_gt_zero, triangleNetEdges)

  /** Creates a rectangular reticulate of width by height squares
   *
   * @param width size must be greater then 0
   * @param height size must be greater then 0
   */
  def pattern_4444(width: Int, height: Int): Either[String, Tiling] =
    rectangular(width, height, width_gt_zero, height_gt_zero, squareNetEdges)

  /** Creates a rectangular reticulate of width by height hexagons
   *
   * @param width  size must be greater then 0
   * @param height size must be greater then 0
   */
  def pattern_666(width: Int, height: Int): Either[String, Tiling] =
    rectangular(width, height, width_gt_zero, height_gt_zero, hexagonNetEdges)

  /** Creates a triangle of triangles of given side */
  def pattern_333333_triangle(side: Int): Either[String, Tiling] =
    triangular(side, side_gt_zero, triangleTriangleEdges)

  /** Creates a trianguloid of hexagons of given side */
  def pattern_666_trianguloid(side: Int): Either[String, Tiling] =
    triangular(side, side_gt_zero, hexTrianguloidEdges)

  /** Creates an octagonal reticulate of width by height octagons
   *
   * @param width size must be greater then 0
   * @param height size must be greater then 0
   */
  def pattern_488(width: Int, height: Int): Either[String, Tiling] =
    rectangular(width, height, width_gt_zero, height_gt_zero, octagonNetEdges)

  private def pattern_33344_33434_altEdges(x: Int, y: Int): List[Edge] =
    val startingNode: Int =
      (x + 1) * (y + 1) * 4 + 1
    val couples: IndexedSeq[(Node, Node)] =
      (for
        i <- 0 until x
        j <- 0 until y
        h = (i + (x + 1) * j) * 4 + 1
        g = (i + x * j) * 2
        additional1 = Node(startingNode + g)
        additional2 = Node(startingNode + g + 1)
        nextRow = (x + 1) * 4
        octagonsNodes =
          List(3, 2, 4, 7, 5 + nextRow, 4 + nextRow, 2 + nextRow, 1 + nextRow)
            .map(n => Node(h + n))
            .rotateLeft(if i % 2 == 0 ^ j % 2 == 0 then 0 else 2)
        connectors = octagonsNodes.take(4).map((additional1, _)) ++ octagonsNodes.drop(4).map((additional2, _))
      yield (additional1, additional2) :: connectors)
        .flatten
    octagonNetEdges(x, y) ++ couples.toList.map(Edge(_))

  /** Creates a [(3₃.4₂);(3₂.4.3.4)] pattern of width by height octagons by filling an octagonal reticulate 
   *
   * @param width size must be greater then 0
   * @param height size must be greater then 0
   */
  def pattern_33344_33434_alt(width: Int, height: Int): Either[String, Tiling] =
    rectangular(width, height, width_gt_zero, height_gt_zero, pattern_33344_33434_altEdges)

  /** Builds variants of triangle grid by emptying hex according to function
   *
   * @param x number of triangles on the x-axis
   * @param y number of triangles on the y-axis
   * @param f function telling if given ij node must be deleted or not
   */
  private def triangleNetVariant(x: Int, y: Int)(f: (Int, Int) => Boolean): List[Edge] =
    val h: Int =
      x / 2
    val emptyNodes: IndexedSeq[Node] =
      for
        i <- 1 to h + 1
        j <- 0 to y
        if f(i, j)
      yield Node(i + (h + 1) * j)
    val edgesEmptied: List[Edge] =
      triangleNetEdges(x, y).withoutNodes(emptyNodes.toList)
    val edges: List[Edge] =
      edgesEmptied.withoutNodes(edgesEmptied.pendantNodes)

    def cutCorners(edges: List[Edge], bridges: List[Node], corners: List[Node]): List[Edge] =
      if edges.nodes.intersect(bridges).isEmpty then
        edges.withoutNodes(corners)
      else
        edges

    val bottomLeft: List[Node] =
      List(1 + (y - 2) * (h + 1), 3 + y * (h + 1)).map(Node(_))
    val blCorners: List[Node] =
      List(1 + (y - 1) * (h + 1), 2 + y * (h + 1), 1 + y * (h + 1)).map(Node(_))
    val topRight: List[Node] =
      List(3 * (h + 1), h - 1).map(Node(_))
    val trCorners: List[Node] =
      List(2 * (h + 1), h, h + 1).map(Node(_))

    cutCorners(cutCorners(edges, bottomLeft, blCorners), topRight, trCorners).compact

  /** Builds variants of square grid by converting square to two triangles according to function
   *
   * @param x number of triangles on the x-axis
   * @param y number of triangles on the y-axis
   * @param f function telling if given i node in a row must be converted or not
   * @param g function telling if given j row must be reversed or not
   */
  private def squareNetVariant(x: Int, y: Int)(f: Int => Boolean)(g: Int => Boolean): List[Edge] =
    val start: List[Edge] =
      squareNetEdges(x, y)
    val additional: IndexedSeq[Edge] =
      for
        j <- 0 until y
        isRowEven = g(j)
        i <- 0 until x
        h = i + 1 + (x + 1) * j
        if f(i) ^ isRowEven
      yield
        if isRowEven then
          Edge(Node(h), Node(h + x + 2))
        else
          Edge(Node(h + 1), Node(h + x + 1))
    start ++ additional

  def pattern_33434(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_gt_zero,
      height_gt_zero,
      squareNetVariant(_, _)(_ % 2 < 1)(_ % 2 == 0)
    )

  def pattern_33344_33434(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_gt_zero,
      height_gt_zero,
      squareNetVariant(_, _)(_ % 4 < 2)(_ % 2 == 0)
    )

  /** @see https://probabilitysports.com/tilings.html?u=0&n=3&t=53 */
  def pattern_2x33344_33434(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_gt_zero,
      height_gt_zero,
      squareNetVariant(_, _)(_ % 6 < 3)(_ % 2 == 0)
    )

  /** @see https://probabilitysports.com/tilings.html?u=0&n=3&t=23 */
  def pattern_33344_33434_4444(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_gt_zero,
      height_gt_zero,
      squareNetVariant(_, _)(_ % 3 == 0)(_ % 3 == 0)
    )

  /** Uniform tessellation (3.6.3.6) (t=2, e=1) */
  def pattern_3636_other(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      _.refineEither[EvenAndGreater[4] DescribedAs "Width should be even and greater than 4"],
      height_gt_one,
      triangleNetVariant(_, _)(_ % 2 == 1 && _ % 2 == 1)
    )

  /** Uniform Tessellation (3₄.6) (t=3, e=3) */
  def pattern_33336(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_even_gt_two,
      height_gt_zero,
      triangleNetVariant(_, _)((i, j) => (i + 2 * j) % 7 == 0)
    )

  /** 2-uniform Tessellation (3₄.6; 3₂.6₂) (t=2, e=4) */
  def pattern_33336_3366(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_even_gt_two,
      height_gt_one,
      triangleNetVariant(_, _)((i, j) => (i + 3 * j) % 5 == 0)
    )

  /** 2-uniform Tessellation (3.6.3.6; 3₂.6₂) (t=2, e=3) */
  def pattern_3636_3366(width: Int, height: Int): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_even_gt_two,
      height_gt_one,
      triangleNetVariant(_, _)((i, j) => (i + 2 * j) % 4 == 0)
    )

  /** 3-uniform Tessellation (3₂.6₂; 3.6.3.6; 6₃) (t=4, e=5) */
  def pattern_3366_3636_666_alt(width: Int, height: Int): Either[String, Tiling] =
    val f: (Int, Int) => Boolean =
      (i, j) => j % 5 match
        case e if e == 2 || e == 4 => i % 5 == (e / 2 + 1) || i % 5 == (e / 2 - 1)
        case _                     => (i + 2 * j) % 5 == 0
    rectangular(
      width,
      height,
      width_even_gt_two,
      height_gt_one,
      triangleNetVariant(_, _)(f)
    )

  /** Creates a rectangular net of triangles and empties some of them to hexagons according to function */
  def triangleRect(width: Int, height: Int)(f: (Int, Int) => Boolean): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_even_gt_zero,
      height_gt_zero,
      triangleNetVariant(_, _)(f)
    )

  /** Gets nodes of a single hex on the hexagonNet
   *
   * @param x number of hexagons on the x-axis
   * @param i cell coordinate on the x-axis (0 first)
   * @param j cell coordinate on the y-axis (0 first)
   */
  private def hexagonNetCellNodes(x: Int, i: Int, j: Int): List[Int] =
    for
      q <- List(0, 1)
      p <- List(0, 1, 2)
    yield (j + q) * ((x + 1) * 2) + i * 2 + p + 1 - q

  /** Builds variants of hex grid by filling hex with 3₆ according to function
   *
   * @param x number of hexagons on the x-axis
   * @param y number of hexagons on the y-axis
   * @param f function telling if given xy hex must be filled or not
   */
  private def hexagonNetVariant(x: Int, y: Int)(f: (Int, Int) => Boolean): List[Edge] =
    val totNodes: Int =
      (y + 1) * 2 * (x + 1) - 2
    val hexNodes: IndexedSeq[List[Int]] =
      for
        i <- 0 until x
        j <- 0 until y
        if f(i, j)
      yield hexagonNetCellNodes(x, i, j)
    val (edges, _): (List[Edge], Int) =
      hexNodes.foldLeft((hexagonNetEdges(x, y), totNodes + 1))({
        case ((startingEdges, centre), hex) =>
          (hex.foldLeft(startingEdges)((h, hexNode) => h :+ Edge((Node(centre), Node(hexNode)))), centre + 1)
      })
    edges.compact

  /** Creates a rectangular net of hexagons and fill some of them with six triangles according to function */
  def hexagonRect(width: Int, height: Int)(f: (Int, Int) => Boolean): Either[String, Tiling] =
    rectangular(
      width,
      height,
      width_even_gt_zero,
      height_gt_zero,
      hexagonNetVariant(_, _)(f)
    )
