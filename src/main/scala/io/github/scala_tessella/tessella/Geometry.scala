package io.github.scala_tessella.tessella

import io.github.scala_tessella.tessella.GeometryBase.{Box9D, LineSegment9D, Point9D}
import Topology.{Edge, Node}
import utility.Utils.mapValues2

import scala.annotation.targetName

/** Associations of node and spatial 2D coordinates */
type Coords = Map[Node, Point9D]

/** Methods to help the spatial representation of a tiling */
object Geometry extends Accuracy:

  /** Standard unit of angular measure */
  opaque type Radian = Double

  /** Companion object for [[Radian]] */
  object Radian:

    /** Create a [[Radian]] from a `Double` */
    def apply(d: Double): Radian =
      d

    /**
     * @see [[https://tauday.com/]]
     */
    val TAU: Radian = Radian(6.283185307179586)
    val TAU_2: Radian = Radian(Math.PI)
    val TAU_3: Radian = TAU / 3
    val TAU_4: Radian = Radian(1.5707963267948966)
    val TAU_6: Radian = TAU_2 / 3

  extension (r: Radian)

    /** @return the underlying `Double` */
    def toDouble: Double =
      r

    @targetName("plus")
    def +(that: Radian): Radian =
      r + that

    @targetName("minus")
    def -(that: Radian): Radian =
      r - that

    @targetName("times")
    def *(i: Int): Radian =
      r * Radian(i)

    @targetName("divide")
    def /(i: Int): Radian =
      r / Radian(i)

  /** Spatial coordinates for the first two nodes of a [[Tiling]] */
  val startingCoords: Coords =
    Map(Node(1) -> Point9D(0, 0), Node(2) -> Point9D(1, 0))

  extension (coords: Coords)

    /** New coordinates guaranteeing that the third node of a [[Tiling]] has always a positive y value */
    def flipVertically: Coords =
      coords.get(Node(3)) match
        case Some(point) if point.y < 0 => coords.mapValues2(_.flipVertically)
        case _                          => coords

    /** New coordinates aligned with starting */
    def alignWithStart: Coords =
      (for
        one <- coords.get(Node(1))
        two <- coords.get(Node(2))
      yield coords.mapValues2(_.alignWithStart(one, two)))
        .map(_.flipVertically).getOrElse(coords)

  extension (edge: Edge)

    /** Creates a `LineSegment2D` of the given coordinates */
    def toSegment(coords: Coords): LineSegment9D =
      LineSegment9D(coords(edge.lesserNode), coords(edge.greaterNode))

  extension (edges: List[Edge])

    /** Creates a sequence of `LineSegment2D` of the given coordinates */
    def toSegments(coords: Coords): List[LineSegment9D] =
      edges.map(_.toSegment(coords))

    /** Creates a bounding `Box2D` containing all edges with the given coordinates
     *
     * @param coords      map with the points of each node
     * @param enlargement extra space at each side
     */
    def toBox(coords: Coords, enlargement: Double = 0.0): Box9D =
      val points: List[Point9D] =
        edges.nodes.map(coords)
      val xs: List[Double] =
        points.map(_.x)
      val ys: List[Double] =
        points.map(_.y)
      Box9D(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)
