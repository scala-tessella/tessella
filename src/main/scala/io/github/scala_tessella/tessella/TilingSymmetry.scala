package io.github.scala_tessella.tessella

import Topology.{Edge, Node}

import io.github.scala_tessella.ring_seq.RingSeq.*

/** Methods dealing with the symmetry of a tiling and of its perimeter */
object TilingSymmetry:

  extension (tiling: Tiling)

    /** Perimeter rotational symmetry */
    def perimeterRotationalSymmetry: Int =
      tiling.orderedPerimeterMinorVertices.rotationalSymmetry

    /** Perimeter reflectional symmetry */
    def perimeterReflectionSymmetry: Int =
      tiling.orderedPerimeterMinorVertices.symmetry

    private def perimeterDistinctSymmetricNodesRaw(indices: List[Index]): Vector[Node] =
      val h: Index =
        indices.head
      tiling.perimeter.toRingNodes
        .sliceO(h - Math.floor(tiling.perimeterLength / indices.size / 2.0).toInt, h + 1)

    /** Distinct symmetric nodes of the perimeter */
    def perimeterDistinctSymmetricNodes: Vector[Node] =
      tiling.orderedPerimeterMinorVertices.symmetryIndices match
        case Nil => tiling.perimeter.toRingNodes.take(tiling.perimeterLength / perimeterRotationalSymmetry)
        case indices@_ :: _ => perimeterDistinctSymmetricNodesRaw(indices)

    // @todo: polygons are not ordered, so a perimeter 343 vertex is the same of a 334, possible precision loss
    private def nestedInnerRawSymmetry(f: Vector[List[Int]] => Int, isStrict: Boolean = true): List[Int] =
      val newF: List[tiling.PolygonEdges] => Int =
      {
        case _ :: Nil => Int.MaxValue
        case polygons =>
          val peri: Vector[Node] =
            tiling.internalPerimeter(polygons)
          //        val fulls: Vector[Vector[Polygon]] =
          //          peri.map(nonPerimeterOrderedPolygons(_))
          val partials: Vector[List[Int]] =
            peri.map(node => polygons.filter(_.nodes.contains(node)).map(_.size).sorted)
          f(partials)
      }

      tiling.inner(_.maxByOption(_.size).map(newF).getOrElse(Int.MaxValue), isStrict)

    private def nestedSymmetry(isStrict: Boolean = true): Int =
      (perimeterReflectionSymmetry :: nestedInnerRawSymmetry(_.symmetry, isStrict)).min

    private def nestedRotationalSymmetry(isStrict: Boolean = true): Int =
      (perimeterRotationalSymmetry :: nestedInnerRawSymmetry(_.rotationalSymmetry, isStrict)).min

    /** Rotational symmetry */
    def countRotationalSymmetries: Int =
      nestedRotationalSymmetry(isStrict = true)

    /** Reflectional symmetry */
    def countSymmetries: Int =
      nestedSymmetry(isStrict = true)
