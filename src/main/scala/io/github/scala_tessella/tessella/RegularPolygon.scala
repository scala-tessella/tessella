package io.github.scala_tessella.tessella

import Geometry.Radian
import Geometry.Radian.{TAU, TAU_2}
import utility.Utils.{mapByValue, originalsOnly, toMap2}
import io.github.scala_tessella.ring_seq.RingSeq.*

import scala.annotation.targetName
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Try

/** Methods to deal with regular polygons in a tiling */
object RegularPolygon:

  /** Unit regular polygon of given number of sides */
  opaque type Polygon = Int

  /** Companion object for [[Polygon]] */
  object Polygon:

    /** Create a [[Polygon]] of given sides
     *
     * @param sides number of sides
     * @throws IllegalArgumentException if sides <= 2
     */
    def apply(sides: Int): Polygon =
      if sides > 2 then sides
      else throw new IllegalArgumentException(s"Invalid number of sides: $sides")

  /** Polygons ordered by ascending number of sides */
  object PolygonOrdering extends Ordering[Polygon] :

    def compare(a: Polygon, b: Polygon): Int =
      a compare b

  extension (sides: Polygon)

    /** @return the underlying number of sides */
    def toSides: Int =
      sides

    /** Measure of each interior angle. */
    def alpha: Radian =
      TAU_2 * (sides - 2) / sides

    /** Measure of the area with unit side. */
    def area: Double =
      sides / 4.0 / Math.tan((TAU_2 / sides).toDouble)

  /** Sequence of polygons ordered by ascending number of sides */
  object PolygonsSeqOrdering extends Ordering[Seq[Polygon]] :

    def compare(a: Seq[Polygon], b: Seq[Polygon]): Int =
      a compare b

  /** Sequence of polygons ordered by ascending max number of sides */
  object PolygonsMaxOrdering extends Ordering[Seq[Polygon]] :

    def compare(a: Seq[Polygon], b: Seq[Polygon]): Int =
      a.max compare b.max

  extension (polygons: Vector[Polygon])

    /** Measure of the sum of each interior angles joined at a vertex. */
    def alphaSum: Radian =
      Radian(polygons.map(_.alpha.toDouble).sum)

  /** Sequence of adjacent regular polygons around a vertex */
  opaque type Vertex = Vector[Polygon]

  /** Companion object for [[Vertex]] */
  object Vertex extends Accuracy :

    /** Creates a [[Vertex]] from a sequence of ordered adjacent polygons
     *
     * @throws IllegalArgumentException if the sum of the interior angles exceeds a full circle
     */
    def apply(polygons: Vector[Polygon]): Vertex =
      if polygons.isValid then polygons
      else throw new IllegalArgumentException(s"More than full vertex: $polygons")

    def apply(polygons: Polygon*): Vertex =
      apply(polygons.toVector)

    @targetName("apply_int")
    def apply(sides: Int*): Vertex =
      apply(sides.map(Polygon(_)).toVector)

    /** Creates an empty [[Vertex]] */
    def empty: Vertex =
      apply(Vector.empty)

    /** Finds all combinations of adjacent regular polygons filling an angle
     *
     * @param l list of regular polygons by side
     * @example {{{
     *          val l = List(Polygon(3), Polygon(4))
     *          val fv = Vertex.fullVerticesByRegularPolygons(l)
     *          fv // contains 3.3.3.4.4
     *          }}}
     */
    def fullVerticesBy(l: List[Polygon]): List[Vector[Polygon]] =

      /**
       * @param beta     vertex exterior angle
       * @param polygons sizes of regular polygon to be tested, ordered by increasing number of sides
       * @param acc      accumulator
       * @return combination of polygons
       */
      def loop(beta: Radian, polygons: List[Polygon], acc: List[Vector[Polygon]]): List[Vector[Polygon]] =
        polygons
          .filter(_.alpha.toDouble <|~= beta.toDouble)
          .flatMap(polygon =>
            val polygonAlpha: Radian =
              polygon.alpha
            val newAcc: List[Vector[Polygon]] =
              acc.map(polygon +: _)
            beta -polygonAlpha match
              case zero if zero.toDouble ~= 0.0 => newAcc
              case newBeta =>
                loop(
                  newBeta,
                  polygons.filter(_.alpha.toDouble <|~= polygonAlpha.toDouble),
                  newAcc
                )
          )

      loop(TAU, l.sorted(PolygonOrdering).distinct, List(Vector()))

    /** Finds all combinations of adjacent regular polygons filling an angle included permutations
     *
     * @param l list of regular polygons by side
     * @example {{{
     *          val l = List(Polygon(3), Polygon(4))
     *          val fvp = Vertex.fullVerticesPermutations(l)
     *          fvp // contains both 3.3.3.4.4 and 3.3.4.3.4
     *          }}}
     */
    def fullVerticesWithPermutationsBy(l: List[Polygon]): List[Vector[Polygon]] =
      fullVerticesBy(l).flatMap(vertex =>
        vertex.permutations.toList
          .distinctBy(_.isRotationOrReflectionOf(vertex))
          .map(_.rotationsAndReflections.min)
      )

    /** Exhaustive list of different combinations of adjacent regular polygons giving full vertices, in their minor version. */
    lazy val fullMinors: List[Vector[Polygon]] =
      fullVerticesWithPermutationsBy((3 to 50).toList.map(Polygon(_))).sorted(PolygonsMaxOrdering)

    /** Full vertices with just one kind of polygon */
    lazy val monoHedrals: List[Vertex] =
      fullMinors.filter(_.distinct.size == 1).map(Vertex(_))

    /** Exhaustive ordered list of unit regular polygons that can be part of a tessellation. */
    lazy val tessellablePolygons: List[Polygon] =
      fullMinors.flatten.distinct.sorted(PolygonOrdering)

    /** Finds a tessellable polygon of given sides */
    def maybeTessellablePolygon(sides: Int): Option[Polygon] =
      tessellablePolygons.find(_ == sides)

    /** Associations of tessellable polygons and area */
    lazy val tessellableAreas: Map[Polygon, Double] =
      tessellablePolygons.toMap2(_.area)

    /** Associations of set of full polygons and minor version. */
    lazy val fullToMinor: Map[Vector[Polygon], Vector[Polygon]] =
      fullMinors.toMap2(_.rotationsAndReflections.toVector.distinct).mapByValue

    /** Exhaustive list of different combinations of adjacent regular polygons that are subsets of full vertices. */
    lazy val fillables: List[Vector[Polygon]] =
      val circularSlides: List[List[Vector[Polygon]]] =
        for
          full <- fullMinors
          size <- 1 until full.size
        yield full.slidingO(size).toList
      circularSlides.flatten.distinct.flatMap(_.reversions).distinct.sorted(PolygonsMaxOrdering)

    /** Associations of fillable vertex and the vertices filling it. */
    lazy val fillingsOf: Map[Vertex, List[Vertex]] =
      fillables.map(fillable => Vertex(fillable) -> fillable.fillings).toMap

  private object VertexOrdering extends Ordering[Vertex]:

    def compare(a: Vertex, b: Vertex): Int =
      a compare b

  private object SizeOrdering extends Ordering[Vertex]:

    def compare(a: Vertex, b: Vertex): Int =
      a.sizeCompare(b)

  /** Vertices ordered by size first, then by contained polygons. */
  object VertexSizeOrdering extends Ordering[Vertex]:

    def compare(a: Vertex, b: Vertex): Int =
      SizeOrdering.orElse(VertexOrdering).compare(a, b)

  extension (polygons: Vertex)

    /** @return the underlying sequence of ordered polygons */
    def toPolygons: Vector[Polygon] =
      polygons

    /** Checks if vertex is full */
    def isFull: Boolean =
      polygons.size >= 3 && Vertex.fullToMinor.contains(polygons)

    /** Checks if vertex is fillable */
    def isFillable: Boolean =
      Vertex.fillables.contains(polygons)

    /** Calculates internal angle */
    def alpha: Radian =
      polygons.alphaSum

    /** Checks if vertex is less than full
     *
     * @note it might be less than full, but not fillable because no polygon is fitting
     */
    def isLessThanFull: Boolean =
      alpha.toDouble < TAU.toDouble

    /** Checks if vertex is fillable or less than full */
    def isPartial: Boolean =
      isFillable || isLessThanFull

    /** Checks if valid */
    def isValid: Boolean =
      polygons.size <= 6 && (isFull || isPartial)

    /** Checks if full and reduced to its minor version */
    def isFullMinor: Boolean =
      Vertex.fullMinors.contains(polygons)

    /** Checks if two full vertices are the same, their minor versions identical */
    def isSameFull(that: Vertex): Boolean =
      (for
        full1 <- Vertex.fullToMinor.get(polygons)
        full2 <- Vertex.fullToMinor.get(that)
      yield full1 == full2).getOrElse(false)

    private def conditionalSliding(size: Int): Iterator[Vertex] =
      if isFull then polygons.slidingO(size)
      else polygons.sliding(size)

    /** Checks if contained in another */
    def isContainedIn(container: Vertex): Boolean =
      (Vertex.fullToMinor.get(polygons), Vertex.fullToMinor.get(container)) match
        case (Some(f), Some(o)) => f == o
        case (Some(_), None)    => false
        case _ =>
          polygons.size <= container.size
            && container.reversions.flatMap(_.conditionalSliding(polygons.size)).contains(polygons)

    /** Checks if remainder of an internal vertex to form a full one */
    def isRemainderOf(internal: Vertex, full: Vertex): Boolean =
      full.isSameFull(Vertex(polygons ++ internal))

    /** All full vertices containing the vertex */
    def fullContainers: List[Vertex] =
      Vertex.fullMinors.filter(polygons.isContainedIn)

    /** All the fillable vertices that could be added to form a full vertex. */
    def fillings: List[Vertex] =
      Vertex.fillables.filter(fillablePolygons => Try(Vertex(fillablePolygons ++ polygons)).toOption.exists(_.isFull))

  extension (vertices: Vector[Vertex])

    /** Checks if all vertices in the sequence are full */
    def areFull: Boolean =
      vertices.forall(_.isFull)

    /** Only vertices not contained in others. */
    def withoutContained: List[Vertex] =
      vertices.sorted(VertexSizeOrdering).toList.originalsOnly(_.isContainedIn(_))
