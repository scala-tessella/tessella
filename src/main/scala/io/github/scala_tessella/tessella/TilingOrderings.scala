package io.github.scala_tessella.tessella

import Topology.EdgesSizeOrdering

import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering

/** Tilings ordered by polygons count */
object TilingPolygonsCountOrdering extends Ordering[Tiling]:

  def compare(a: Tiling, b: Tiling): Int =
    a.countPolygons compare b.countPolygons

/** Tilings ordered by edges size */
object TilingEdgesSizeOrdering extends Ordering[Tiling]:

  def compare(a: Tiling, b: Tiling): Int =
    EdgesSizeOrdering.compare(a.graphEdges, b.graphEdges)

/** Tilings ordered by min rotation of perimeter polygons */
object TilingMinPerimeterPolygonsRotationOrdering extends Ordering[Tiling]:

  def compare(a: Tiling, b: Tiling): Int =
    a.minPerimeterPolygonsRotation compare b.minPerimeterPolygonsRotation
