package io.github.scala_tessella.tessella
package creation

import Topology.{Edge, Node, NodeOrdering}

/** Contains fast methods to create a [[Tiling]] from repeated layers of rows */
trait Layered extends Reticulate:

  /** Grid of 2 * l triangles with in between an emptied hexagon net
   *
   * @param l length of parallel sides
   * @param h height of the hexagon net
   * @param f to empty hexagons in the net
   */
  private def triHexBase(l: Int, h: Int = 0)(f: (Int, Int) => Boolean): (List[Edge], Node) =
    val topRight: Node =
      Node(l + 2)
    val bottomLeft: Node =
      Node(topRight.toInt * (h + 1) + 1)
    val emptyNodes: IndexedSeq[Node] =
      for
        i <- 1 to topRight.toInt
        j <- 1 to h
        if f(i, j)
      yield Node(i + topRight.toInt * j)
    val edges: List[Edge] =
      triangleNet((l + 1) * 2, h + 1).toOption.get.graphEdges.withoutNodes(topRight :: bottomLeft :: emptyNodes.toList).compact
    (edges, Node(edges.nodes.max(NodeOrdering).toInt - l))

  /** Grid of 2 * l triangles with in between a full hexagon net
   *
   * @param l    length of parallel sides
   * @param h    height of the hexagon net
   * @param step position of starting hexagon
   */
  private def hexAreaRow(l: Int, h: Int = 0, step: Int = 0): (List[Edge], Node) =
    val invertedStep: Int =
      3 - step % 3
    triHexBase(l, h)((i, j) => (i + invertedStep + j % 3) % 3 == 0)

  private val fs: Map[Int, Int => (List[Edge], Node)] = Map(
    3 -> { l => (triangleNet(l * 2, 1).toOption.get.graphEdges, Node(l + 2)) },
    4 -> { l => (squareNet(l, 1).toOption.get.graphEdges, Node(l + 2)) },
    36 -> { triHexBase(_, 1)((i, _) => i % 2 == 1) },
    63 -> { triHexBase(_, 1)((i, _) => i % 2 == 0) },
    336 -> { triHexBase(_, 1)((i, _) => i % 3 == 1) },
    363 -> { triHexBase(_, 1)((i, _) => i % 3 == 0) },
    633 -> { triHexBase(_, 1)((i, _) => i % 3 == 2) },
    3362 -> { hexAreaRow(_, 2, 2) },
    3632 -> { hexAreaRow(_, 2, 1) },
    6332 -> { hexAreaRow(_, 2) }
  )

  private def netVariant(layers: List[Int], width: Int, height: Int): Either[String, Tiling] =
    for
      ls <- width_gt_zero(width)
      rows <- height_gt_zero(height)
      t <- {
        val size = layers.size
        val (edges, _) = (0 until rows).foldLeft((Nil: List[Edge], 1))({
          case ((gg, start), row) =>
            val (t, s) = fs(layers(row % size))(ls)
            (gg ++ t.compactStartingAt(Node(start)), start + s.toInt - 1)
        })
        Tiling.maybe(edges)
      }
    yield
      t  

  /**
   * uniform tessellation (3₃.4₂) (t=2, e=3)
   *
   * @see https://en.wikipedia.org/wiki/Elongated_triangular_tiling
   */
  def elongatedTriangular(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(3, 4), width, height)

  /**
   * uniform tessellation (3.6.3.6) (t=2, e=1)
   *
   * @see https://en.wikipedia.org/wiki/Trihexagonal_tiling
   */
  def triHexagonal(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(36, 63), width, height)

  /**
   * uniform tessellation (3₄.6; 3₃.4₂; 3.4₂.6)
   *
   * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
   */
  def threeUniformOneOneOne7(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 363), width, height)

  /**
   * uniform tessellation (3₃.4₂; 3₂.6₂; 3.4₂.6) (t=5, e=8)
   *
   * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#3-uniform_tilings,_3_vertex_types
   */
  def threeUniformOneOneOne8(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 3632, 4, 6332, 4, 3362), width, height)

  /**
   * uniform tessellation (3₃.4₂; [2x 3₂.6₂]; [2x 3.4₂.6])
   *
   * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_3_vertex_types_(3:1:1)_and_(2:2:1)
   */
  def fiveUniformTwoTwoOne5(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 6332), width, height)

  /**
   * uniform tessellation ([4x 3.6.3.6]; 3.4₂.6)
   *
   * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
   */
  def fiveUniformFourOne2(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 36, 63, 36, 63), width, height)

  /**
   * uniform tessellation ([4x 3.6.3.6]; 3.4₂.6)
   *
   * @see https://en.wikipedia.org/wiki/Euclidean_tilings_by_convex_regular_polygons#5-uniform_tilings,_2_vertex_types_(4:1)_and_(3:2)
   */
  def fiveUniformFourOne3(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 36, 63, 36, 63, 4, 63, 36, 63, 36), width, height)

  /**
   * @see https://probabilitysports.com/tilings.html?u=0&n=5&t=195
   */
  def fiveUniformIssue1(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 3, 4, 36), width, height)

  /**
   * @see https://probabilitysports.com/tilings.html?u=0&n=5&t=194
   */
  def fiveUniformIssue2(width: Int, height: Int): Either[String, Tiling] =
    netVariant(List(4, 3, 4, 36, 4, 3, 4, 36, 4, 3, 4, 63, 4, 3, 4, 63), width, height)
