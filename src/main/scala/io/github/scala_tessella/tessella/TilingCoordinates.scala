package io.github.scala_tessella.tessella

import Geometry.{Box, LineSegment, Point}
import Topology.{Edge, Node}
import utility.Utils.mapValues2

/** Methods to help the spatial representation of a tiling */
object TilingCoordinates:

  /** Associations of node and spatial 2D coordinates */
  type Coords = Map[Node, Point]

  /** Spatial coordinates for the first two nodes of a [[Tiling]] */
  val startingCoords: Coords =
    Map(Node(1) -> Point(), Node(2) -> Point(1, 0))

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

    /** Creates a `LineSegment` of the given coordinates */
    def toSegment(coords: Coords): LineSegment =
      LineSegment(coords(edge.lesserNode), coords(edge.greaterNode))

  extension (edges: List[Edge])

    /** Creates a sequence of `LineSegment` of the given coordinates */
    def toSegments(coords: Coords): List[LineSegment] =
      edges.map(_.toSegment(coords))

    /** Creates a bounding `Box` containing all edges with the given coordinates
     *
     * @param coords      map with the points of each node
     * @param enlargement extra space at each side
     */
    def toBox(coords: Coords, enlargement: Double = 0.0): Box =
      val points: List[Point] =
        edges.nodes.map(coords)
      val xs: List[Double] =
        points.map(_.x)
      val ys: List[Double] =
        points.map(_.y)
      Box(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)
