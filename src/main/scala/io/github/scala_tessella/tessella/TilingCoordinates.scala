package io.github.scala_tessella.tessella

import Geometry.{AngleDegree, Box, BoxReal, LineSegment, LineSegmentReal, Point, PointReal, Radian}
import Geometry.Radian.TAU_2
import Topology.{Edge, Node}
import utility.Utils.{mapValues2, toCouple}
import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO, startAt}
import spire.math.Real
import spire.syntax.isReal.partialOrderOps
import spire.compat.ordering

import scala.annotation.tailrec

/** Methods to help the spatial representation of a tiling */
object TilingCoordinates:

  /** Associations of node and spatial 2D coordinates */
  type Coords = Map[Node, Point]

  type CoordsReal = Map[Node, PointReal]

  /** Spatial coordinates for the first two nodes of a [[Tiling]] */
  private val startingCoords: Coords =
    Map(Node(1) -> Point(), Node(2) -> Point(1, 0))

  private val startingCoordsReal: CoordsReal =
    Map(Node(1) -> PointReal(0, 0), Node(2) -> PointReal(1, 0))

  extension (tiling: Tiling)

    /** Spatial coordinates of a [[Tiling]] */
    def coordinates: Coords =

      @tailrec
      def loop(coords: Coords, polygons: List[tiling.PolygonPath]): Coords =
        polygons.find(_.toPolygonPathNodes.slidingO(2).exists(_.forall(node => coords.contains(node)))) match
          case None => coords
          case Some(polygon) =>
            val pairs: List[Vector[Node]] =
              polygon.toPolygonPathNodes.slidingO(2).toList
            val index: Index =
              pairs.indexWhere(_.forall(node => coords.contains(node)))
            val startingAngle: Radian =
              coords(pairs(index)(0)).angleTo(coords(pairs(index)(1)))
            val alpha: Radian =
              polygon.toPolygon.alpha
            val newCoords: Coords =
              pairs.startAt(index - 1).drop(2).map(_.toCouple).foldLeft((coords, startingAngle))({
                case ((cumulativeCoordinates, angle), (previous, node)) =>
                  val newAngle: Radian =
                    angle + TAU_2 - alpha
                  val newCumulativeCoordinates: Coords =
                    if cumulativeCoordinates.contains(node) then cumulativeCoordinates
                    else cumulativeCoordinates + (node -> cumulativeCoordinates(previous).plusPolarUnit(newAngle))
                  (newCumulativeCoordinates, newAngle)
              })._1
            val newPolygons: List[tiling.PolygonPath] =
              polygons.diff(List(polygon)).filter(_.toPolygonPathNodes.exists(node => !newCoords.contains(node)))
            loop(newCoords, newPolygons)

      if tiling.graphEdges.isEmpty then
        Map()
      else
        loop(startingCoords, tiling.orientedPolygons).flipVertically

    def coordinatesReal: CoordsReal =

      @tailrec
      def loop(coords: CoordsReal, polygons: List[tiling.PolygonPath]): CoordsReal =
        polygons.find(_.toPolygonPathNodes.slidingO(2).exists(_.forall(node => coords.contains(node)))) match
          case None => coords
          case Some(polygon) =>
            val pairs: List[Vector[Node]] =
              polygon.toPolygonPathNodes.slidingO(2).toList
            val index: Index =
              pairs.indexWhere(_.forall(node => coords.contains(node)))
            val startingAngle: AngleDegree =
              coords(pairs(index)(0)).angleTo(coords(pairs(index)(1)))
            val alpha: AngleDegree =
              polygon.toPolygon.alphaDegree
            val newCoords: CoordsReal =
              pairs.startAt(index - 1).drop(2).map(_.toCouple).foldLeft((coords, startingAngle))({
                case ((cumulativeCoordinates, angle), (previous, node)) =>
                  val newAngle: AngleDegree =
                    angle + AngleDegree(180) - alpha
                  val newCumulativeCoordinates: CoordsReal =
                    if cumulativeCoordinates.contains(node) then cumulativeCoordinates
                    else cumulativeCoordinates + (node -> cumulativeCoordinates(previous).plusPolarUnit(newAngle))
                  (newCumulativeCoordinates, newAngle)
              })._1
            val newPolygons: List[tiling.PolygonPath] =
              polygons.diff(List(polygon)).filter(_.toPolygonPathNodes.exists(node => !newCoords.contains(node)))
            loop(newCoords, newPolygons)

      if tiling.graphEdges.isEmpty then
        Map ()
      else
        loop(startingCoordsReal, tiling.orientedPolygons).flipVerticallyReal

  extension (nodes: Vector[Node])

    def pointsFrom(angles: Map[Node, Radian]): Vector[Point] =
      nodes.scanLeft((Point(1, 0), TAU_2: Radian))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + TAU_2)
      }).map((point, _) => point).tail

    def pointsRealFrom(angles: Map[Node, AngleDegree]): Vector[PointReal] =
      nodes.scanLeft((PointReal(1, 0), AngleDegree(180)))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + AngleDegree(180))
      }).map((point, _) => point).tail

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

  extension (coords: CoordsReal)

    /** New coordinates guaranteeing that the third node of a [[Tiling]] has always a positive y value */
    def flipVerticallyReal: CoordsReal =
      coords.get(Node(3)) match
        case Some(point) if point.y < 0 => coords.mapValues2(_.flipVertically)
        case _ => coords

  extension (edge: Edge)

    /** Creates a `LineSegment` of the given coordinates */
    def toSegment(coords: Coords): LineSegment =
      LineSegment(coords(edge.lesserNode), coords(edge.greaterNode))
      
    def toSegmentReal(coords: CoordsReal): LineSegmentReal =
      LineSegmentReal(coords(edge.lesserNode), coords(edge.greaterNode))

  extension (edges: List[Edge])

    /** Creates a sequence of `LineSegment` of the given coordinates */
    def toSegments(coords: Coords): List[LineSegment] =
      edges.map(_.toSegment(coords))

    def toSegmentsReal(coords: CoordsReal): List[LineSegmentReal] =
      edges.map(_.toSegmentReal(coords))

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

    def toBoxReal(coords: CoordsReal, enlargement: Real = 0.0): BoxReal =
      val points: List[PointReal] =
        edges.nodes.map(coords)
      val xs: List[Real] =
        points.map(_.x)
      val ys: List[Real] =
        points.map(_.y)
      BoxReal(xs.min - enlargement, xs.max + enlargement, ys.min - enlargement, ys.max + enlargement)

