package io.github.scala_tessella.tessella
package creation

import Geometry.Radian
import TilingGrowth.OtherNodeStrategy.BEFORE_PERIMETER
import RegularPolygon.{Polygon, Vertex}
import TilingSymmetry.perimeterRotationalSymmetry
import Topology.{Edge, Node}
import utility.UtilsOption.getDefined

import io.github.scala_tessella.ring_seq.RingSeq.{Index, applyO}

object Symmetric:

  private val mappedPrimeDivisors: Map[Int, List[Int]] =
    Map(
      2 -> List(2),
      3 -> List(3),
      4 -> List(2),
      6 -> List(3, 2),
      8 -> List(2),
      12 -> List(3, 2)
    )

  /** Polygons that can be added */
  lazy val additionalPolygons: List[Polygon] =
    List(3, 4, 6, 8, 12).map(Polygon(_))

  /** Polygons that can be added, except for octagon */
  lazy val additionalPolygonsWithoutOctagon: List[Polygon] =
    additionalPolygons.filterNot(_.toSides == 8)

  extension (tiling: Tiling)

    /** Finds a list of perimeter node sets and a list of polygons for symmetric expansion */
    def rotationalPerimeterNodeSets: (List[List[Node]], List[Polygon]) =
      tiling.perimeterRotationalSymmetry match
        case 1 => (Nil, Nil)
        case axes =>
          val perimeterAngles: Vector[Radian] =
            tiling.orderedPerimeterAngles
          val indexWhereMinExternalAngle: Index =
            perimeterAngles.indexOf(perimeterAngles.maxBy(_.toDouble))
          val gap: Int =
            perimeterAngles.size / axes
          val nodesForMaxSymmetry: List[Node] =
            (0 until axes).toList.map(n => tiling.perimeter.toRingNodes.applyO(n * gap + indexWhereMinExternalAngle))
          val perimeterNodeSets: List[List[Node]] =
            mappedPrimeDivisors(axes).map(divisor => (0 until axes by (axes / divisor)).toList.map(nodesForMaxSymmetry(_)))
          val polygons: List[Polygon] =
            if axes == 2 || axes == 4 || axes == 8 then
              additionalPolygons
            else
              additionalPolygonsWithoutOctagon
          val aGrowablePerimeterNode: Node =
            perimeterNodeSets.head.head
          val vertexToBeGrown: Vertex =
            tiling.minorVertexAt(aGrowablePerimeterNode)
          val fillingPolygons: List[Polygon] =
            polygons
              .intersect(
                Vertex.fillingsOf.get(vertexToBeGrown) match
                  case Some(value) => value.flatMap(_.toPolygons)
                  case None        => Nil
              )
              .distinct
          (perimeterNodeSets, fillingPolygons)

    /** Creates a sequence of new symmetric tilings, adding all combinations of possible expansions */
    def expansionStep: List[Tiling] =
      val (perimeterNodeSets, polygons): (List[List[Node]], List[Polygon]) =
        rotationalPerimeterNodeSets
      val tilings: List[Option[Tiling]] =
        for
          polygon         <- polygons
          startFromBefore <- List(true, false)
          perimeterNodes  <- perimeterNodeSets
        yield
          perimeterNodes.foldLeft(Option(tiling): Option[Tiling])(
            (maybeTiling, node) =>
              for
                t         <- maybeTiling
                otherNode <- if startFromBefore then tiling.perimeter.beforeO(node) else tiling.perimeter.afterO(node)
                grown     <- t.maybeGrowEdge(Edge(node, otherNode), polygon, BEFORE_PERIMETER).toOption
              yield
                grown
         )
      Tiling.distinctSafe(tilings.getDefined)

    /** Expands by symmetric axes
     *
     * @param steps number of expansions
     * @param validity function to filter tilings
     */
    def expansion(steps: Int, validity: Tiling => Boolean = _ => true): List[Tiling] =
      (0 until steps).foldLeft(List(tiling))((tilings, step) =>
        Tiling.distinctSafe(tilings.flatMap(_.expansionStep)).filter(validity)
      )
