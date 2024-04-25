package io.github.scala_tessella.tessella
package creation

import RegularPolygon.{Polygon, Vertex}
import TilingGrowth.OtherNodeStrategy.BEFORE_PERIMETER
import Topology.Edge

import scala.util.Random

object Randomic:

  extension (tiling: Tiling)

    def isEmpty: Boolean =
      tiling.graphEdges.isEmpty

    def randomStep(polygons: Option[List[Polygon]] = None): Option[Tiling] =
      val randomPolygons: List[Polygon] =
        Random.shuffle(polygons.getOrElse(Vertex.tessellablePolygons))
      if isEmpty then
        Option(Tiling.fromPolygon(randomPolygons.head))
      else
        val combinations: List[(Edge, Polygon)] =
          for
            polygon <- randomPolygons
            edge    <- tiling.perimeter.toRingEdges
          yield
            (edge, polygon)
        combinations
          .view
          .map(tiling.maybeGrowEdge(_, _, BEFORE_PERIMETER))
          .find(_.isRight)
          .map(_.toOption.get)

    def randomSteps(steps: Int, polygons: Option[List[Polygon]] = None): Option[Tiling] =
      (0 until steps).foldLeft(Option(tiling))((maybeTiling, _) => maybeTiling.flatMap(_.randomStep(polygons)))
