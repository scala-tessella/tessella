package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.{TAU, TAU_2}
import GeometryBase.{Box, Point, SimplePolygon}
import TilingGrowth.*
import RegularPolygon.{Polygon, PolygonsSeqOrdering, Vertex}
import TilingErrorMessages.*
import Topology.*
import creation.{Layered, Quadratic, Uni4Hex, Uni5Hex, UniHex, UniTriangle}
import utility.Utils.*

import io.github.scala_tessella.ring_seq.RingSeq.*

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering
import scala.util.Try

/** Undirected connected graph representing a finite tessellation of unit regular polygons
 *
 * @param edges the graph edges
 * @note being `private` a [[Tiling]] cannot be created outside the class, thus ensuring edges validation
 */
case class Tiling private(edges: List[Edge]) extends Graph(edges) with Ordered[Tiling]:

  override def toString: String =
    s"Tiling(${edges.stringify})"

  /** Compilable output */
  def toCompilableString: String =
    s"Tiling.maybe(${edges.stringify}).toOption.get"

  /** A tessellation unit regular polygon seen as a collection of edges */
  type PolygonEdges = List[Edge]

  /** Circular path of adjacent graph nodes delimiting a tessellation unit regular polygon */
  opaque type PolygonPath = Vector[Node]

  /** Companion object for [[PolygonPath]] */
  object PolygonPath:

    /** Creates a [[PolygonPath]] faster without validity checks. */
    def unsafe(nodes: Vector[Node]): PolygonPath =
      nodes

    private def internalPathMsg(node1: Node, node2: Node): String =
      s"Invalid regular polygon path: node $node1 and node $node2 are connected internally"

    /* once checked that it is a ring, checks that is also a tiling polygon */
    private def check(nodes: Vector[Node], sides: Int): PolygonPath =
      val halfSize: Double =
        sides / 2.0
      val halfSizeFloored: Int =
        halfSize.toInt
      val pairs: Vector[(Node, Node)] =
        if halfSize == halfSizeFloored then
          val (firstHalf, secondHalf) = nodes.splitAt(2)
          firstHalf.zip(secondHalf)
        else
          nodes.indices.map(index => (nodes(index), nodes.applyO(index + halfSizeFloored))).toVector
      pairs.find((from, to) => edges.distance(from, to) < halfSizeFloored) match
        case Some((from, to)) => throw new IllegalArgumentException(internalPathMsg(from, to))
        case _                => nodes

    /** Creates a [[PolygonPath]] from a sequence of adjacent nodes ordered from start to end.
     *
     * @throws IllegalArgumentException if sequence don't represent the circular path formed by the vertices of a regular polygon of the [[Tiling]]
     */
    def apply(nodes: Vector[Node]): PolygonPath =
      val sides: Int =
        Polygon(nodes.size).toSides
      check(Path.check(nodes, _.slidingO(2)), sides)

  extension (nodes: PolygonPath)

    /** @return the underlying sequence of ordered adjacent nodes */
    def toPolygonPathNodes: Vector[Node] =
      nodes

    /** Converts to generic polygon */
    def toPolygon: Polygon =
      Polygon(nodes.size)

    private def toRingPath: RingPath =
      RingPath.unsafe(nodes)

    /** Converts to edges */
    def toPolygonEdges: PolygonEdges =
      toRingPath.toRingEdges.toList

    private def checkShared(path: Vector[Node], f: Seq[Node] => Boolean): Boolean =
      f(nodes.intersect(path))

    private def hasSharedNodesWith(that: Vector[Node]): Boolean =
      checkShared(that, _.nonEmpty)

    /** Checks that polygon does not touch any of the given nodes */
    def hasNoSharedNodesWith(that: Vector[Node]): Boolean =
      !hasSharedNodesWith(that)

    private def hasSharedSideWith(that: PolygonPath): Boolean =
      checkShared(that, _.size == 2)

    private def startFrom(node: Node): PolygonPath =
      nodes.indexOf(node) match
        case -1 => nodes
        case i  => nodes.startAt(i)

  extension (paths: List[PolygonPath])

    private def toPolygonsEdges: List[PolygonEdges] =
      paths.map(_.toPolygonEdges)

    private def perimeterOrderedAt(start: Node): Vector[PolygonPath] =

      @tailrec
      def loop(unordered: List[PolygonPath], ordered: List[PolygonPath]): Vector[PolygonPath] =
        val latestOrdered: PolygonPath = ordered.headOption match
          case Some(value) => value
          case None => throw new Error(
            s"""
               |tiling: $toCompilableString
               |start: $start
               |unordered: $unordered
               |ordered: $ordered
               |""".stripMargin
          )
        unordered.find(_.hasSharedSideWith(latestOrdered)) match
          case Some(path) => loop(unordered.filterNot(_ equals path), path :: ordered)
          case None       => ordered.toVector

      val (order, chaos) = paths.partition(_.toPolygonPathNodes.contains(start))
      loop(chaos, order)

    /** Orders the polygons paths around a full node
     *
     * @note method assumes `paths` are just those around the full node
     * @param origin the full node shared by all polygons paths
     * @return the paths without the origin node
     */
    private def toFullOrderedAdjacentPathsAt(origin: Node): Vector[Path] =
      val unorderedPaths: List[Vector[Node]] =
        paths.map(_.toRingPath.startAtNodeO(origin).get.toRingNodes.tail)

      @tailrec
      def loop(unordered: Vector[Vector[Node]], acc: List[Path]): List[Path] =
        val connector: Node =
          acc.head.toNodes.head
        unordered.find(path => path.head.equals(connector) || path.last.equals(connector)) match
          case Some(path) =>
            val oriented: Vector[Node] =
              if path.head.equals(connector) then path.reverse else path
            loop(unordered.filterNot(_.equals(path)), Path.unsafe(oriented) :: acc)
          case _ => acc

      loop(unorderedPaths.tail.toVector, List(Path.unsafe(unorderedPaths.head))).toVector

    /** Orders the polygons around a full node  */
    private def toFullOrderedAdjacentPolygonsAt(node: Node): Vector[Polygon] =
      toFullOrderedAdjacentPathsAt(node).map(path => Polygon(path.toNodes.size + 1))

  extension (paths: Vector[PolygonPath])

    /** Converts to generic polygons */
    def toPolygons: Vector[Polygon] =
      paths.map(_.toPolygon)

  extension (nodes: RingPath)

    private def toPoints2D(angles: Map[Node, Radian]): Vector[Point] =
      nodes.toRingNodes.scanLeft((Point(1, 0), TAU_2: Radian))({
        case ((point, acc), node) => (point.plusPolarUnit(acc), acc + angles(node) + TAU_2)
      }).map((point, _) => point).tail

  extension (polygon: PolygonEdges)

    /** Checks that polygon shares at least one of the given edges */
    def hasSharedSidesWith(otherEdges: List[Edge]): Boolean =
      polygon.intersect(otherEdges).nonEmpty

    private def hasNoSharedSidesWith(otherEdges: List[Edge]): Boolean =
      !hasSharedSidesWith(otherEdges)

  extension (polygons: List[PolygonEdges])

    /** Filters the (unique) perimeter edges
     *
     * @note method assumes `polygons` form a tiling
     */
    def unorderedPerimeterEdges(): List[Edge] =
      polygons.flatten.filterUnique.toList

    def internalPerimeter: Vector[Node] =
      unorderedPerimeterEdges().maybeCircularNodes(_.headOption).get

    private def deeperPolygons: List[PolygonEdges] =
      polygons.filter(polygonEdges =>
        PolygonPath.unsafe(polygonEdges.nodes.toVector).hasNoSharedNodesWith(internalPerimeter)
      )

    private def strictDeeperPolygons: List[PolygonEdges] =
      polygons.filter(_.hasNoSharedSidesWith(RingPath.unsafe(internalPerimeter).toRingEdges.toList))

  /** Each [[PolygonPath]] of the tiling, all with the same orientation */
  lazy val orientedPolygons: List[PolygonPath] =
    tilingOrientedPolygons.getOrElse(Nil).map(ring => PolygonPath.unsafe(ring.toRingNodes))

  /** Perimeter of the tiling */
  lazy val perimeter: RingPath =
    tilingOrientedPerimeter.getOrElse(RingPath(Vector.empty))

  /** Unit length of the perimeter */
  val perimeterLength: Int =
    perimeter.toRingNodes.size
    
  /** All [[PolygonPath]], those touching the perimeter and those that don't */
  private lazy val (perimeterPolygons, innerPolygons) : (List[PolygonPath], List[PolygonPath]) =
    orientedPolygons.partition(_.hasSharedNodesWith(perimeter.toRingNodes))

  /** All [[PolygonPath]], those sharing at least an edge with the perimeter and those that don't */
  private lazy val (strictPerimeterPolygons, strictInnerPolygons): (List[PolygonPath], List[PolygonPath]) =
    orientedPolygons.partition(_.toPolygonEdges.hasSharedSidesWith(perimeter.toRingEdges.toList))

  /** Method to recursively extract info from nested polygons rings, starting from perimeter
   *
   * @param f function transforming polygons ring, each a collection of tilings, into info unit
   * @param isStrict if true, more rings are formed
   * @tparam T info unit
   */
  def inner[T](f: List[List[PolygonEdges]] => T, isStrict: Boolean = true): List[T] =

    val deeper: List[PolygonEdges] => List[PolygonEdges] =
      if isStrict then _.strictDeeperPolygons else _.deeperPolygons

    @tailrec
    def loop(tilings: List[List[PolygonEdges]], acc: List[T]): List[T] =
      if tilings.isEmpty then
        acc.reverse
      else
        val newTilings: List[List[PolygonEdges]] =
          tilings.flatMap(_.groupConnected(_.hasSharedSidesWith(_)))
        loop(
          newTilings.map(deeper).filter(_.nonEmpty),
          f(newTilings) :: acc
        )

    val start: List[PolygonPath] =
      if isStrict then strictInnerPolygons else innerPolygons
    loop(List(start.toPolygonsEdges), Nil)

  /** Nested polygons rings, each a collection of tilings */
  def nestedTilings(isStrict: Boolean = true): List[List[List[PolygonEdges]]] =
    inner(_.map(identity), isStrict)

  /** Nested perimeters */
  def nestedPerimeters(isStrict: Boolean = true): List[List[RingPath]] =
    List(perimeter) :: inner(_.map(polygons => RingPath.unsafe(polygons.internalPerimeter)), isStrict)


  private type OuterValue[T] = (List[PolygonEdges], List[PolygonPath], List[List[PolygonEdges]], List[T])

  /** Method to recursively extract info from multiple outer polygons rings, each originating from a different node
   *
   * @param origins the different origins
   * @param f function transforming the newest polygon ring and the cumulative past rings into info unit
   * @param isStrict if true, more rings are formed
   * @tparam T info unit
   */
  def outerFrom[T](origins: List[Node],
                   f: (List[PolygonEdges], List[List[PolygonEdges]]) => T,
                   isStrict: Boolean = true): Map[Node, List[T]] =

    val strictFilter: (PolygonPath, List[Edge]) => Boolean =
      if isStrict then _.toPolygonEdges.hasSharedSidesWith(_)
      else (polygonPath, edgeList) => polygonPath.hasSharedNodesWith(edgeList.nodes.toVector)

    @tailrec
    def loop(nodes: Map[Node, OuterValue[T]]): Map[Node, List[T]] =
      if nodes.values.forall((polygons, _, _, _) => polygons.isEmpty)
        then nodes.map({ case (node, (_, _, _, acc)) => node -> acc.reverse })
      else
        loop(
          nodes.map({ case (node, (polygons, outerPolygons, polygonsLayers, acc)) =>
            if polygons.isEmpty then
              node -> (polygons, outerPolygons, polygonsLayers, acc)
            else
              val perimeterEdges: List[Edge] =
                polygons.unorderedPerimeterEdges()
              val (newInner, newOuter): (List[PolygonPath], List[PolygonPath]) =
                outerPolygons.partition(strictFilter(_, perimeterEdges))
              node ->
                (newInner.toPolygonsEdges, newOuter, polygons :: polygonsLayers, f(polygons, polygonsLayers) :: acc)
          })
        )

    val start: Map[Node, OuterValue[T]] =
      origins.map(origin =>
        val polygons: List[PolygonPath] =
          polygonsAt(origin)
        origin -> (polygons.toPolygonsEdges, orientedPolygons.diff(polygons), Nil, Nil)
      ).toMap
    loop(start)

  /** Outer polygons rings from a single node origin */
  def outerPolygonsFromSingle(origin: Node, isStrict: Boolean = true): List[List[PolygonEdges]] =
    outerFrom(List(origin), (polygons, _) => polygons, isStrict).values.head

  private def outerTilingsFromSingle(origin: Node, isStrict: Boolean = true): List[List[List[PolygonEdges]]] =
    outerFrom(List(origin), (polygons, acc) => polygons ++ acc.flatten, isStrict).values.head
      .map(_.groupConnected(_.hasSharedSidesWith(_)))

  /** Outer perimeters from a single node origin */
  def outerPerimetersFromSingle(origin: Node, isStrict: Boolean = true): List[List[RingPath]] =
    outerTilingsFromSingle(origin, isStrict).map(_.map(polygons => RingPath.unsafe(polygons.internalPerimeter)))

  /** Associations of perimeter node and ordered adjacent polygon paths */
  lazy val perimeterOrderedPolygonPaths: Map[Node, Vector[PolygonPath]] =
    perimeter.toRingNodes.slidingO(2)
      .map(_.toCouple)
      .map((prev, node) =>
        node ->
          perimeterPolygons
            .filter(_.toPolygonPathNodes.contains(node))
            .perimeterOrderedAt(prev)
            .map(_.startFrom(node))
      )
      .toMap

  /** Associations of perimeter node and ordered adjacent polygons */
  lazy val perimeterOrderedPolygons: Map[Node, Vector[Polygon]] =
    perimeterOrderedPolygonPaths.mapValues2(_.toPolygons)

  /** Associations of perimeter node and ordered adjacent nodes */
  private lazy val perimeterOrderedAdjacentNodes: Map[Node, Vector[Node]] =
    perimeterOrderedPolygonPaths.mapValues2(paths => paths.map(_(1)) :+ paths.last.last)

  /** Associations of perimeter node and vertex angle */
  lazy val perimeterAngles: Map[Node, Radian] =
    perimeterOrderedPolygons.mapValues2(_.alphaSum)

  /** Spatial coordinates of the perimeter only
   *
   * @note they differ from the points found as a whole tiling in [[Tiling.coords]]
   */
  val perimeterPoints2D: Vector[Point] =
    perimeter.toPoints2D(perimeterAngles)

  /** Associations of perimeter node and spatial coordinate */
  lazy val perimeterCoords: Coords =
    perimeter.toRingNodes.zip(perimeterPoints2D).toMap

  /** Checks there are no multiple perimeter nodes at the same spatial coordinates */
  def hasPerimeterWithDistinctVertices: Boolean =
    edges.isEmpty || perimeterCoords.values.toVector.areAllDistinct

  /** Perimeter 2D polygon */
  lazy val perimeterSimplePolygon2D: SimplePolygon =
    SimplePolygon(perimeterPoints2D.toList)

  /** Checks there are no intersecting perimeter edges */
  def hasPerimeterNotSelfIntersecting: Boolean =
    edges.isEmpty || perimeter.toRingNodes.sizeIs < 13 || !perimeterSimplePolygon2D.isSelfIntersecting

  /** Filters the invalid perimeter vertices */
  def invalidPerimeterVertices: Vector[Node] =
    perimeter.toRingNodes.filterNot(node =>
      Try(Vertex(perimeterOrderedPolygons(node))).toOption match
        case Some(vertex) => vertex.isPartial
        case _            => false
    )

  /** Checks there are no invalid perimeter vertices */
  def hasValidPerimeterVertices: Boolean =
    invalidPerimeterVertices.isEmpty

  /** Ordered perimeter vertices in their minor form */
  def orderedPerimeterMinorVertices: Vector[Vertex] =
    perimeter.toRingNodes.map(node => Vertex(perimeterOrderedPolygons(node).reversions.min(PolygonsSeqOrdering)))

  /** Ordered perimeter angles */
  def orderedPerimeterAngles: Vector[Radian] =
    perimeter.toRingNodes.map(perimeterAngles(_))

  /** Minimal rotated and reflected version of the vertices insisting on each perimeter node */
  def minPerimeterPolygonsRotation: Vector[Vector[Int]] =
    orderedPerimeterMinorVertices.map(_.toPolygons.map(_.toSides)).rotationsAndReflections.min

  override def compare(that: Tiling): Int =
    TilingPolygonsCountOrdering
      .orElse(TilingEdgesSizeOrdering)
      .orElse(TilingMinPerimeterPolygonsRotationOrdering)
      .compare(this, that)

  private def orderedRoundedPerimeterAngles: Vector[Long] =
    orderedPerimeterAngles.map(_.toDouble.rounded())

  override def equals(any: Any): Boolean =

    def areSizesEqual(t1: List[PolygonEdges], t2: List[PolygonEdges]): Boolean =
      EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(t1.flatten, t2.flatten) == 0
        && EdgesSizeMapOrdering.compare(t1, t2) == 0

    @tailrec
    def loop(inner1: List[List[PolygonEdges]], inner2: List[List[PolygonEdges]]): Boolean =
      if inner1.isEmpty && inner2.isEmpty then
        true
      else
        val tilings1: List[List[PolygonEdges]] =
          inner1.flatMap(_.groupConnected(_.hasSharedSidesWith(_)))
        val tilings2: List[List[PolygonEdges]] =
          inner2.flatMap(_.groupConnected(_.hasSharedSidesWith(_)))
        tilings1.sizeCompare(tilings2) == 0
          && areSizesEqual(tilings1.flatten, tilings2.flatten)
          && loop(tilings1.map(_.strictDeeperPolygons), tilings2.map(_.strictDeeperPolygons))

    any match
      case that: Tiling =>
        EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(this.graphEdges, that.graphEdges) == 0 &&
          this.orderedRoundedPerimeterAngles.isRotationOrReflectionOf(that.orderedRoundedPerimeterAngles) &&
          loop(
            List(this.strictInnerPolygons.toPolygonsEdges.filter(_.nonEmpty)),
            List(that.strictInnerPolygons.toPolygonsEdges.filter(_.nonEmpty))
          )
      case _ => false

  /** Finds the 2D box */
  def toBox: Box =
    edges.toBox(coords)

  /** Number of polygons in the tiling */
  def countPolygons: Int =
    orientedPolygons.size

  /** Associations of tiling node and spatial coordinate */
  lazy val coords: Coords =

    @tailrec
    def loop(coordinates: Coords, polygons: List[PolygonPath]): Coords =
      polygons.find(_.slidingO(2).exists(_.forall(node => coordinates.contains(node)))) match
        case None => coordinates
        case Some(polygon) =>
          val pairs: List[Vector[Node]] =
            polygon.slidingO(2).toList
          val index: Index =
            pairs.indexWhere(_.forall(node => coordinates.contains(node)))
          val startingAngle: Radian =
            coordinates(pairs(index)(0)).angleTo(coordinates(pairs(index)(1)))
          val alpha: Radian =
            polygon.toPolygon.alpha
          val newCoordinates: Coords =
            pairs.startAt(index - 1).drop(2).map(_.toCouple).foldLeft((coordinates, startingAngle))({
              case ((cumulativeCoordinates, angle), (previous, node)) =>
                val newAngle: Radian =
                  angle + TAU_2 - alpha
                val newCumulativeCoordinates: Coords =
                  if cumulativeCoordinates.contains(node) then cumulativeCoordinates
                  else cumulativeCoordinates + (node -> cumulativeCoordinates(previous).plusPolarUnit(newAngle))
                (newCumulativeCoordinates, newAngle)
            })._1
          loop(newCoordinates, polygons.diff(polygon).filter(_.exists(node => !newCoordinates.contains(node))))

    if edges.isEmpty then
      Map()
    else
      loop(startingCoords, orientedPolygons).flipVertically

  private def polygonsAt(node: Node): List[PolygonPath] =
    orientedPolygons.filter(_.toPolygonPathNodes.contains(node))

  private lazy val nonPerimeterNodes: List[Node] =
    graphNodes.diff(perimeter.toRingNodes)

  /** Associations of non perimeter node and its ordered adjacent polygons */
  lazy val nonPerimeterOrderedPolygons: Map[Node, Vector[Polygon]] =
    nonPerimeterNodes.toMap2(node => polygonsAt(node).toFullOrderedAdjacentPolygonsAt(node))

  /** Associations of non perimeter node and its ordered adjacent nodes */
  lazy val nonPerimeterOrderedAdjacentNodes: Map[Node, Vector[Node]] =
    nonPerimeterNodes.toMap2(node => polygonsAt(node).toFullOrderedAdjacentPathsAt(node).map(_.toNodes.head))

  /** Associations of node and its ordered adjacent nodes */
  lazy val orderedAdjacentNodes: Map[Node, Vector[Node]] =
    perimeterOrderedAdjacentNodes ++ nonPerimeterOrderedAdjacentNodes

  /** Filters invalid non perimeter nodes */
  def invalidFullVertices: List[Node] =
    nonPerimeterNodes.filterNot(node =>
      Try(Vertex(nonPerimeterOrderedPolygons(node))).toOption match
        case Some(vertex) => vertex.isFull
        case _            => false
    )

  /** Checks if all non perimeter nodes are valid */
  def hasValidFullVertices: Boolean =
    invalidFullVertices.isEmpty

  private def sizedPolygons: Map[Polygon, List[Vector[Node]]] =
    orientedPolygons.map(_.toPolygonPathNodes).groupBy(_.size).mapKeys(Polygon(_))

  /** Associations of polygon and count */
  private def groupHedrals: Map[Polygon, Int] =
    sizedPolygons.mapValues2(_.size)

  /** Number of different polygons in the tiling */
  def hedrality: Int =
    sizedPolygons.size

  /** Area of the tiling */
  def area: Double =
    groupHedrals.map((polygon, count) => Vertex.tessellableAreas(polygon) * count).sum

//  /** Alternative method for the area of the tiling */
//  def areaAlt: Double =
//    Math.abs(perimeterSimplePolygon2D.area())

  /** @see https://en.wikipedia.org/wiki/Compactness_measure#Examples */
  def compactness: Double =
    area / (perimeterLength * perimeterLength / (2.0 * TAU.toDouble))

  /** Minor version of the vertex at a given node */
  def minorVertexAt(node: Node): Vertex =
    perimeterOrderedPolygons.get(node) match
      case Some(polygons) => Vertex(polygons.reversions.min(PolygonsSeqOrdering))
      case None           => Vertex(Vertex.fullToMinor(nonPerimeterOrderedPolygons(node)))

  /** Association of node and minor vertex. */
  private lazy val allMinorVertices: Map[Node, Vertex] =
    graphNodes.toMap2(minorVertexAt)

  /** Association of minor full vertex and nodes. */
  lazy val groupGonals: Map[Vertex, Iterable[Node]] =
    val grouped: Map[Vertex, Iterable[Node]] =
      allMinorVertices.groupByValues
    val containersOnly: List[Vertex] =
      grouped.keys.toVector.withoutContained
    grouped.filter((vertex, _) => containersOnly.contains(vertex))

  /** Number of different full vertices in the tiling */
  def gonality: Int =
    groupGonals.size

  private def edgesToUnsafeTiling: Either[GrowthLeft, List[Edge]] => Either[String, Tiling] = {
    case Left((leftEdges, f)) => Left(f(Tiling(leftEdges)))
    case Right(newEdges)      => Right(Tiling(edges ++ newEdges))
  }

  /** Tries to build a new `Tiling` with an additional polygon adjacent to a perimeter node
   *
   * @param node                perimeter node
   * @param polygon             added polygon
   * @param otherNodeStrategies methods to choose the other adjacent perimeter node,
   *                            identifying the edge that the polygon is sharing
   * @return Either the tiling or a failure message
   */
  def maybeGrowNode(node: Node, polygon: Polygon, otherNodeStrategies: OtherNodeStrategy *): Either[String, Tiling] =
    edgesToUnsafeTiling(this.edgesFromPerimeterNodeGrowth(node, polygon, otherNodeStrategies.toList))

  /** Tries to build a new `Tiling` with an additional polygon sharing a perimeter edge
   *
   * @param edge                perimeter edge
   * @param polygon             added polygon
   * @param otherNodeStrategies methods to choose from which edge's node the new edges start
   * @return Either the tiling or a failure message
   */
  def maybeGrowEdge(edge: Edge, polygon: Polygon, otherNodeStrategies: OtherNodeStrategy *): Either[String, Tiling] =
    edgesToUnsafeTiling(this.edgesFromPerimeterEdgeGrowth(edge, polygon, otherNodeStrategies.toList))

  private def leftToStringLeft: Either[GrowthLeft, Tiling] => Either[String, Tiling] = {
    case Left((leftEdges, f)) => Left(f(Tiling(leftEdges)))
    case Right(tiling)        => Right(tiling)
  }

  /** Tries to build a new `Tiling` with an additional vertex adjacent to a perimeter node
   *
   * @param node                perimeter node
   * @param vertex              added vertex
   * @param otherNodeStrategies methods to choose the other adjacent perimeter node,
   *                            identifying the edge that the first polygon of the vertex is sharing
   * @return Either the tiling or a failure message
   */
  def maybeGrowNode(node: Node, vertex: Vertex, otherNodeStrategies: OtherNodeStrategy *): Either[String, Tiling] =
    leftToStringLeft(
      TilingGrowth.genericGrowPerimeterNodeByVertex(this)(
        node,
        vertex,
        (t, newEdges) => Tiling(t.edges ++ newEdges),
        otherNodeStrategies.toList
      )
    )

  /** Nodes ordered by lesser perimeter angle */
  object MinAngleNodeOrdering extends Ordering[Node]:

    def compare(a: Node, b: Node): Int =
      perimeterAngles(b).toDouble compare perimeterAngles(a).toDouble

  /** Edges ordered by lesser perimeter angle at lesser perimeter angle node */
  private object MinAngleEdgeNodeOrdering extends Ordering[Edge]:

    def compare(edge1: Edge, edge2: Edge): Int =
      MinAngleNodeOrdering.compare(
        edge1.nodes.min(MinAngleNodeOrdering),
        edge2.nodes.min(MinAngleNodeOrdering)
      )

  /** Edges ordered by lesser perimeter angle at greater perimeter angle node */
  private object MaxAngleEdgeNodeOrdering extends Ordering[Edge]:

    def compare(edge1: Edge, edge2: Edge): Int =
      MinAngleNodeOrdering.compare(
        edge1.nodes.min(MinAngleNodeOrdering.reverse),
        edge2.nodes.min(MinAngleNodeOrdering.reverse)
      )

  /** Edges ordered first by lesser perimeter angle at lesser perimeter angle node, then by lesser at greater */
  object MinAngleEdgeOrdering extends Ordering[Edge]:

    def compare(a: Edge, b: Edge): Int =
      MinAngleEdgeNodeOrdering.orElse(MaxAngleEdgeNodeOrdering).compare(a, b)

/** Companion object for [[Tiling]] */
object Tiling extends UniTriangle with UniHex with Uni4Hex with Uni5Hex with Layered with Quadratic:

  /** The empty tiling */
  def empty: Tiling =
    Tiling(Nil)

  /** A tiling made of the edges of the given polygon
   *
   * @param sides polygon or its number of sides
   */
  def fromPolygon(sides: Polygon | Int): Tiling =
    val s: Int =
      (sides: @unchecked) match
        case i: Int  => i
    Tiling(Vector.range(1, s + 1).map(Node(_)).toEdgesO.toList)

  /** A tiling made of the edges of the given adjacent polygons
   *
   * @param vertex a partial or full vertex
   */
  def fromVertex(vertex: Vertex): Tiling =
    vertex.toPolygons match
      case e if e.isEmpty => Tiling.empty
      case polygons =>
        val pairedTo1: List[Int] =
          polygons.foldLeft(List(2))((l, polygon) => (l.head + (polygon.toSides - 2)) :: l)
        val lastNode: Int =
          pairedTo1.head
        val allBut1: Vector[Node] =
          (2 to (if vertex.isFull then lastNode - 1 else lastNode)).toVector.map(Node(_))
        val without1: List[Edge] =
          (if vertex.isFull then Node(lastNode - 1) +: allBut1 else allBut1).toEdges.toList
        val with1: List[Edge] =
          (if vertex.isFull then pairedTo1.tail else pairedTo1)
            .map(ordinal => Edge((Node(1), Node(ordinal))))
        Tiling(without1 ++ with1)

  /** A tiling made of a [[FullVertex]] */
  def fromFullVertex(fullVertex: FullVertex): Tiling =
    fromVertex(fullVertex.vertex)
    
  private def fromGraphUnsafe(graph: Graph) =
    Tiling(graph.graphEdges)

  private val invalidDegrees: Degree => Boolean =
    degree => isPendant(degree) || degree.toInt > Degree(6).toInt

  private val invalidPerimeterDegrees: Degree => Boolean =
    !isThread(_)

  private def maybePerimeterEdges(edges: List[Edge]): Either[String, List[Edge]] =
    val tentative: Graph =
      Graph(edges.distinct)
    if tentative.graphEdges.allDegrees.values.exists(invalidPerimeterDegrees) then
      Left(
        tentative.invalidDegreeErrMsg(invalidPerimeterDegrees, "Perimeter must have each node connected to exactly 2")
      )
    else if !tentative.isConnected then
      Left(tentative.disconnectedErrMsg("Perimeter"))
    else
      Right(tentative.graphEdges)

  /** Tries to create a `Tiling` with validation.
   *
   * @param edges list of edges
   * @return Either the tiling or a failure message
   */
  def maybe(edges: List[Edge]): Either[String, Tiling] =
    val tentative: Graph =
      Graph(edges.distinct)
    if !tentative.isCompacted then
      Left(interruptedErrMsg(tentative))
    else if tentative.graphEdges.allDegrees.values.exists(invalidDegrees) then
      Left(tentative.invalidDegreeErrMsg(invalidDegrees, "Tiling must have each node connected to min 2 and max 6"))
    else if !tentative.isConnected then
      Left(tentative.disconnectedErrMsg("Tiling"))
    else
      val unsafeTiling: Tiling =
        fromGraphUnsafe(tentative)
      if unsafeTiling.perimeter.toRingNodes.isEmpty then
        maybePerimeterEdges(unsafeTiling.edges).map(_ => Tiling.empty)
      else if !unsafeTiling.hasValidPerimeterVertices then
        Left(invalidPerimeterVertexErrMsg(unsafeTiling))
      else if !unsafeTiling.hasValidFullVertices then
        Left(invalidFullVertexErrMsg(unsafeTiling))
      else if !unsafeTiling.hasPerimeterWithDistinctVertices then
        Left(invalidVertexCoordsErrMsg(unsafeTiling))
      else if !unsafeTiling.hasPerimeterNotSelfIntersecting then
        Left(invalidIntersectionErrMsg(unsafeTiling))
      else
        Right(unsafeTiling)

  /** Tries to create a `Tiling` with validation.
   *
   * @param edges sequence of edges
   * @return Either the tiling or a failure message
   */
  def maybe(edges: Edge*): Either[String, Tiling] =
    maybe(edges.toList)

  /** Filters out duplicate tilings.
   *
   * @note Reimplementing the wheel, but test shows that factory `distinct` can fail.
   */
  def distinctSafe(tilings: List[Tiling]): List[Tiling] =
    val found: mutable.ArrayBuffer[Tiling] =
      mutable.ArrayBuffer.empty[Tiling]

    @tailrec
    def loop(remaining: List[Tiling]): List[Tiling] =
      remaining match
        case Nil => found.toList
        case h :: t =>
          if !found.contains(h) then found += h
          loop(t)

    loop(tilings)
