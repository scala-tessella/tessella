package io.github.scala_tessella.tessella
package creation

import RegularPolygon.{Polygon, Vertex}
import TilingGrowth.OtherNodeStrategy.BEFORE_PERIMETER
import Topology.Edge

import scala.util.Random

/** Contains slow methods to create a [[Tiling]] with growth governed by randomic functions */
object Randomic:

  extension (polygons: Vertex)

    private def isContainedInAtLeastOne(containers: List[Vertex]): Boolean =
      containers.exists(polygons.isContainedIn)

    private def isContainedInPattern(pattern: Pattern): Boolean =
      isContainedInAtLeastOne(pattern.distinctVertices.map(_.vertex))

  extension (tiling: Tiling)

    private def isEmpty: Boolean =
      tiling.graphEdges.isEmpty

    /** Tries adding a random polygon, with an optional addition validity clause
     *
     * @param maybePolygons polygons that can be added, if None all tessellable polygons
     * @param validity      function to filter tilings
     */
    def randomStep(maybePolygons: Option[List[Polygon]] = None,
                   validity: Tiling => Boolean = _ => true): Option[Tiling] =
      val polygons: List[Polygon] =
        maybePolygons.getOrElse(Vertex.tessellablePolygons)
      if isEmpty then
        Option(Tiling.fromPolygon(polygons(Random.nextInt(polygons.size))))
      else
        val combinations: List[(Edge, Polygon)] =
          for
            polygon <- polygons
            edge    <- tiling.perimeter.toRingEdges
          yield
            (edge, polygon)
        Random.shuffle(combinations)
          .view
          .map(tiling.maybeGrowEdge(_, _, BEFORE_PERIMETER))
          .find(maybeTiling => maybeTiling.isRight && validity(maybeTiling.toOption.get))
          .map(_.toOption.get)

    /** Tries adding sequentially random polygons, with an optional addition validity clause
     *
     * @param steps         number of additions
     * @param maybePolygons polygons that can be added, if None all tessellable polygons
     * @param validity      function to filter tilings
     * @return
     */
    def randomSteps(steps: Int,
                    maybePolygons: Option[List[Polygon]] = None,
                    validity: Tiling => Boolean = _ => true): Option[Tiling] =
      (0 until steps).foldLeft(Option(tiling))((maybeTiling, _) =>
        maybeTiling.flatMap(_.randomStep(maybePolygons, validity))
      )

    /** Tries adding sequentially random polygons, within a given pattern
     *
     * @param steps   number of additions
     * @param pattern a [[Pattern]] each new polygon must follow
     * @return
     */
    def randomStepsWithinPattern(steps: Int, pattern: Pattern): Option[Tiling] =
      val polygons: List[Polygon] =
        pattern.vertices.flatMap(_.vertex.toPolygons).distinct
      val validity: Tiling => Boolean =
        _.orderedPerimeterMinorVertices.forall(_.isContainedInPattern(pattern))
      randomSteps(steps, Option(polygons), validity)
