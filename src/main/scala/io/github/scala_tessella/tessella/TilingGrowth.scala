package io.github.scala_tessella.tessella

import TilingCoordinates.*
import Geometry.*
import Geometry.Radian.TAU_2
import RegularPolygon.{Polygon, Vertex}
import Tiling.{empty, fromPolygon}
import TilingErrorMessages.*
import TilingGrowth.OtherNodeStrategy.*
import TilingGrowth.PerimeterStrategy.{LOWEST_ORDINAL, NARROWEST_ANGLE}
import Topology.{BeforeAfterOrdering, Edge, EdgeOrdering, Node, NodeOrdering}
import utility.Utils.toCouple

import io.github.scala_tessella.ring_seq.RingSeq.{Index, reflectAt}

import scala.util.Try

/** Methods dealing with the growth of an existing tiling */
object TilingGrowth:

  /** Enumerates the possible strategies to find an adjacent perimeter node */
  enum OtherNodeStrategy:

    case FIXED(from: Node) extends OtherNodeStrategy
    case AFTER_PERIMETER   extends OtherNodeStrategy
    case BEFORE_PERIMETER  extends OtherNodeStrategy
    case HIGHER_ORDINAL    extends OtherNodeStrategy
    case LOWER_ORDINAL     extends OtherNodeStrategy
    case WIDER_ANGLE       extends OtherNodeStrategy
    case NARROWER_ANGLE    extends OtherNodeStrategy

    def toOrdering(angleOrdering: Ordering[Node]): Node => Ordering[Node] =
      (this: @unchecked) match
        case AFTER_PERIMETER  => BeforeAfterOrdering(_).reverse
        case BEFORE_PERIMETER => BeforeAfterOrdering(_)
        case HIGHER_ORDINAL   => _ => NodeOrdering.reverse
        case LOWER_ORDINAL    => _ => NodeOrdering
        case WIDER_ANGLE      => _ => angleOrdering.reverse
        case NARROWER_ANGLE   => _ => angleOrdering

  /** Enumerates the possible strategies to find a perimeter node */
  enum PerimeterStrategy:

    case HIGHEST_ORDINAL extends PerimeterStrategy
    case LOWEST_ORDINAL  extends PerimeterStrategy
    case WIDEST_ANGLE    extends PerimeterStrategy
    case NARROWEST_ANGLE extends PerimeterStrategy

    def toOrdering[T](ordering: Ordering[T], angleOrdering: Ordering[T]): Ordering[T] =
      this match
        case HIGHEST_ORDINAL => ordering.reverse
        case LOWEST_ORDINAL  => ordering
        case WIDEST_ANGLE    => angleOrdering.reverse
        case NARROWEST_ANGLE => angleOrdering


  /** Strategy to find a perimeter node, an adjacent perimeter node and if biggest polygons first */
  type FullStrategy = (List[PerimeterStrategy], List[OtherNodeStrategy], Boolean)

  private def rawFullVertex[T](steps: Int,
                               fullVertex: FullVertex,
                               empty: T,
                               f: (Range, Either[String, Tiling], (Either[String, Tiling], Int) => Either[String, Tiling]) => T)
                              (perimeterStrategies: List[PerimeterStrategy],
                               otherNodeStrategies: List[OtherNodeStrategy],
                               biggestPolygonsFirst: Boolean = true): T =
    if steps < 1 then
      empty
    else
      val polygons: List[Polygon] =
        fullVertex.minor.vertex.toPolygons.toList.distinct
      val orderedPolygons: List[Polygon] =
        if biggestPolygonsFirst then polygons.reverse else polygons
      val start: Either[String, Tiling] =
        Right(Tiling.fromPolygon(orderedPolygons.head))
      f(0 until steps - 1, start, (maybeTiling, step) =>
        maybeTiling.flatMap(t =>
          val sortedNodes: Vector[Node] =
            t.sortedPerimeterNodes(perimeterStrategies *)
          val searchable: List[(Node, Polygon)] =
            sortedNodes.toList.flatMap(node => orderedPolygons.map(polygon => (node, polygon)))
          searchable
            .view
            .filter((node, polygon) => t.isFillableBy(fullVertex, node, polygon, otherNodeStrategies))
            .map((node, polygon) => t.maybeGrowNode(node, polygon, otherNodeStrategies *))
            .find(_.isRight)
            .getOrElse(Left(t.noFillablePerimeterEdgesErrMsg(polygons.head, step)))
        )
      )

  /** Tries to grow a monogonal tiling of given size, from a full vertex, according to a strategy */
  def growFullVertex(size: Int, fullVertex: FullVertex, fullStrategy: FullStrategy): Either[String, Tiling] =
    rawFullVertex(size, fullVertex, Right(Tiling.empty), _.foldLeft(_)(_)).tupled(fullStrategy)

  /** Tries to grow a sequence of monogonal tilings up to given size, from a full vertex, according to a strategy */
  def scanFullVertex(steps: Int, fullVertex: FullVertex, fullStrategy: FullStrategy): IndexedSeq[Either[String, Tiling]] =
    rawFullVertex(steps, fullVertex, IndexedSeq.empty, _.scanLeft(_)(_)).tupled(fullStrategy)

  /** Left side of an `Either`, when growth fails */
  type GrowthLeft = (List[Edge], Tiling => String)

  extension (pathNodes: Vector[Node])

    private def toCoords(node: Node, polygon: Polygon, coords: Coords, startFromBefore: Boolean = true): Coords =
      if pathNodes.diff(coords.keys.toList).isEmpty then
        coords
      else
        val startingAngle: Radian =
          coords(node).angleTo(coords(pathNodes.head))
        val angle: Radian =
          polygon.alpha
        //          Polygon(nodes.toNodes.size + 1).alpha
        val orientedAngle: Radian =
          if startFromBefore then angle else Radian(-angle.toDouble)
        pathNodes.foldLeft((coords, node, startingAngle))({
          case ((accCoords, currentNode, acc), nextNode) =>
            val cumulativeCoords: Coords =
              if accCoords.contains(nextNode) then accCoords
              else accCoords + (nextNode -> accCoords(currentNode).plusPolarUnit(acc))
            (cumulativeCoords, nextNode, acc + orientedAngle + TAU_2)
        })._1

    private def toCoordsReal(node: Node, polygon: Polygon, coords: CoordsReal, startFromBefore: Boolean = true): CoordsReal =
      if pathNodes.diff(coords.keys.toList).isEmpty then
        coords
      else
        val startingAngle: AngleDegree =
          coords(node).angleTo(coords(pathNodes.head))
        val angle: AngleDegree =
          polygon.alphaDegree
        //          Polygon(nodes.toNodes.size + 1).alpha
        val orientedAngle: AngleDegree =
          if startFromBefore then angle else angle.inverted
        pathNodes.foldLeft((coords, node, startingAngle))({
          case ((accCoords, currentNode, acc), nextNode) =>
            val cumulativeCoords: CoordsReal =
              if accCoords.contains(nextNode) then accCoords
              else accCoords + (nextNode -> accCoords(currentNode).plusPolarUnit(acc))
            (cumulativeCoords, nextNode, acc + orientedAngle + AngleDegree(180))
        })._1

  extension (tiling: Tiling)

    private def combinedOrdering[T](orderings: List[Ordering[T]]): Ordering[T] =
      orderings match
        case h :: t => t.foldLeft(h)(_.orElse(_))
        case Nil    => throw new Error("no strategy given")

    private def sortedPerimeterNodes(perimeterStrategies: PerimeterStrategy *): Vector[Node] =
      val combinedStrategy: Ordering[Node] =
        combinedOrdering(perimeterStrategies.toList.map(_.toOrdering(NodeOrdering, tiling.MinAngleNodeOrdering)))
      tiling.perimeter.toRingNodes.sorted(combinedStrategy)

    private def sortedPerimeterEdges(perimeterStrategies: PerimeterStrategy *): Vector[Edge] =
      val combinedStrategy: Ordering[Edge] =
        combinedOrdering(perimeterStrategies.toList.map(_.toOrdering(EdgeOrdering, tiling.MinAngleEdgeOrdering)))
      tiling.perimeter.toRingEdges.toVector.sorted(combinedStrategy)

    private def sortedPerimeterNodesPair(pair: (Node, Node), otherNodeStrategies: OtherNodeStrategy *): (Node, Node) =
      val (before, after): (Node, Node) = pair
      val combinedStrategy: Ordering[Node] =
        combinedOrdering(otherNodeStrategies.toList.map(_.toOrdering(tiling.MinAngleNodeOrdering)(before)))
      List(before, after).sorted(combinedStrategy).toCouple

    // find the two diverging perimeter paths, originating from node (end) and start
    private def twoDivergingPerimeterPaths(node: Node, polygon: Polygon, startFromBefore: Boolean): List[tiling.Path] =
      val rotatedPerimeter: tiling.RingPath =
        tiling.perimeter.startAtNodeO(node).get
      val couples: List[Vector[Node]] =
        List(rotatedPerimeter.toRingNodes, rotatedPerimeter.toRingNodes.reflectAt(0))
          .map(_.take(polygon.toSides))
      val couplesOriented: List[Vector[Node]] =
        if startFromBefore then couples
        else couples.reverse
      List(couplesOriented.head.init, couplesOriented(1).tail).map(tiling.Path.unsafe(_))

    private def getBeforeStart(start: Node, startFromBefore: Boolean): Node =
      if startFromBefore then tiling.perimeter.afterO(start).get
      else tiling.perimeter.beforeO(start).get
    
    // check that new edges don't touch or cross the perimeter
    private def spatialCheck(node: Node,
                             polygon: Polygon,
                             pathNodes: Vector[Node],
                             end: Node,
                             newEdges: List[Edge],
                             startFromBefore: Boolean): Either[GrowthLeft, List[Edge]] =

      def perimeterCoordsAt(node1: Node, node2: Node): Coords =
        tiling.perimeterCoords.filter({ case (k, _) => k.equals(node1) || k.equals(node2) })

      val start: Node =
        pathNodes.head
      val beforeStart: Node =
        getBeforeStart(start, startFromBefore)
      val newCoords: Coords =
        if pathNodes.size < 2 then
          Map()
        else
          pathNodes
            .toCoords(
              beforeStart,
              polygon,
              perimeterCoordsAt(beforeStart, start),
              startFromBefore
            )
            .filter((n, _) => n >= pathNodes(1))
      newCoords
        .filter((_, point) => tiling.perimeterCoords.values.exists(_.almostEquals(point, LESSER_ACCURACY)))
        .toList match
        case _ :: _ =>
          Left((tiling.graphEdges ++ newEdges, _.invalidVertexCoordsErrMsg))
        case Nil =>
          val newEdgesCoords: Coords =
            newCoords ++ perimeterCoordsAt(end, start)
          val lines: List[LineSegment] =
            newEdges.toSegments(newEdgesCoords)
          val enlargedBox: Box =
            newEdges.toBox(newEdgesCoords, 1.0)
          val perimeterLines: List[LineSegment] =
            tiling.perimeter.toRingEdges.toList.withoutNodes(List(node))
              .toSegments(tiling.perimeterCoords).filter(_.hasEndpointIn(enlargedBox))
          if lines.lesserIntersects(perimeterLines) then
            Left((tiling.graphEdges ++ newEdges, _.invalidIntersectionErrMsg))
          else
            Right(newEdges)

    // check that new edges don't touch or cross the perimeter
    private def spatialCheckNew(node: Node,
                             polygon: Polygon,
                             pathNodes: Vector[Node],
                             end: Node,
                             newEdges: List[Edge],
                             startFromBefore: Boolean): Either[GrowthLeft, List[Edge]] =

      def perimeterCoordsRealAt(node1: Node, node2: Node): CoordsReal =
        tiling.perimeterCoordsReal.filter({ case (k, _) => k.equals(node1) || k.equals(node2) })

      val start: Node =
        pathNodes.head
      val beforeStart: Node =
        getBeforeStart(start, startFromBefore)
      val newCoordsReal: CoordsReal =
        if pathNodes.size < 2 then
          Map()
        else
          pathNodes
            .toCoordsReal(
              beforeStart,
              polygon,
              perimeterCoordsRealAt(beforeStart, start),
              startFromBefore
            )
            .filter((n, _) => n >= pathNodes(1))
      newCoordsReal
        .filter((_, point) => tiling.perimeterCoordsReal.values.exists(_.almostEquals(point)))
        .toList match
        case _ :: _ =>
          Left((tiling.graphEdges ++ newEdges, _.invalidVertexCoordsErrMsg))
        case Nil =>
          val newEdgesCoords: CoordsReal =
            newCoordsReal ++ perimeterCoordsRealAt(end, start)
          val lines: List[LineSegmentReal] =
            newEdges.toSegmentsReal(newEdgesCoords)
          val enlargedBox: BoxReal =
            newEdges.toBoxReal(newEdgesCoords, 1.0)
          val perimeterLines: List[LineSegmentReal] =
            tiling.perimeter.toRingEdges.toList.withoutNodes(List(node))
              .toSegmentsReal(tiling.perimeterCoordsReal).filter(_.hasEndpointIn(enlargedBox))
          if lines.lesserIntersectsReal(perimeterLines) then
            Left((tiling.graphEdges ++ newEdges, _.invalidIntersectionErrMsg))
          else
            Right(newEdges)

    private def pathNodesAndEnd(node: Node, polygon: Polygon, startFromBefore: Boolean): Either[GrowthLeft, (Vector[Node], Node, List[Edge])] =
      val paths: List[Vector[Node]] =
        tiling.twoDivergingPerimeterPaths(node, polygon, startFromBefore).map(_.toNodes)
      val (endIndex, startIndex): (Index, Index) =
        paths
          // finds either a partial or an invalid vertex, the latter will in case fail at anglesCheck
          .map(nodes => nodes.indices.find(i =>
            Try(Vertex(polygon +: tiling.perimeterOrderedPolygons(nodes(i)))).map(!_.isFull).getOrElse(true)
          ).get)
          .toCouple
      val (start, end): (Node, Node) =
        (paths(1)(startIndex), paths.head(endIndex))
      anglesCheck(List(start, end), polygon)
        .map(_ =>
          val nextNodeOrdinal: Int =
            tiling.graphNodes.max(NodeOrdering).toInt + 1
          val middleNodes: Vector[Node] =
            (0 until (polygon.toSides - (startIndex + endIndex + 2))).map(_ + nextNodeOrdinal).map(Node(_)).toVector
          val pathNodes: Vector[Node] =
            start +: middleNodes
          val newEdges: List[Edge] =
            tiling.Path.unsafe(pathNodes :+ end).toPathEdges.toList
          (pathNodes, end, newEdges)
        )

    private def onPerimeterCheck(node: Node): Either[GrowthLeft, ?] =
      if tiling.perimeter.toRingNodes.contains(node) then Right(())
      else Left((tiling.graphEdges, _.addToNonPerimeterNodeErrMsg(node)))

    private def anglesCheck(nodes: List[Node], polygon: Polygon): Either[GrowthLeft, ?] =
      nodes.find(node => Try(Vertex(polygon +: tiling.perimeterOrderedPolygons(node))).isFailure) match
        case Some(node) => Left((tiling.graphEdges, _.addExceedingAngleErrMsg(node, Vertex(polygon))))
        case None       => Right(())

    private def nodeImplementStrategy(otherNodeStrategies: List[OtherNodeStrategy]): Node => Either[GrowthLeft, Boolean] =
      node => {
        val (before, after): (Node, Node) =
          tiling.perimeter.beforeAfterO(node).get
        otherNodeStrategies match
          case FIXED(from) :: _ if from == before => Right(true)
          case FIXED(from) :: _ if from == after  => Right(false)
          case FIXED(from) :: _                   => onPerimeterCheck(from).map(_ => true)
          case _ =>
            val (b, _) = sortedPerimeterNodesPair((before, after), otherNodeStrategies *)
            Right(b == before)
      }

    /** Try to find the edges of a new additional polygon adjacent to a perimeter node
     *
     * @param node                perimeter node
     * @param polygon             added polygon
     * @param otherNodeStrategies method to choose the other adjacent perimeter node,
     *                            identifying the edge that the polygon is sharing
     * @return Either the edges or a failure message
     */
    def edgesFromPerimeterNodeGrowth(node: Node,
                                     polygon: Polygon,
                                     otherNodeStrategies: List[OtherNodeStrategy]): Either[GrowthLeft, List[Edge]] =
      for
        _ <- onPerimeterCheck(node)
        startFromBefore <- nodeImplementStrategy(otherNodeStrategies)(node)
        pathNodesAndEnd <- pathNodesAndEnd(node, polygon, startFromBefore)
        (pathNodes, end, edges) = pathNodesAndEnd
        _ <- spatialCheck(node, polygon, pathNodes, end, edges, startFromBefore)
      yield
        edges

    private def edgeImplementingStrategy(firstSecond: (Node, Node), otherNodeStrategies: List[OtherNodeStrategy]): (Node, Node) =
      val (first, second): (Node, Node) = firstSecond
      otherNodeStrategies match
        case FIXED(from) :: _ if first == from || second != from => firstSecond
        case FIXED(_) :: _                                       => firstSecond.swap
        case _                                                   => sortedPerimeterNodesPair(firstSecond, otherNodeStrategies *)

    /** Try to find the edges of a new additional polygon sharing a perimeter edge
     *
     * @param edge                perimeter edge
     * @param polygon             added polygon
     * @param otherNodeStrategies method to choose from which edge's node the new edges start
     * @return Either the edges or a failure message
     */
    def edgesFromPerimeterEdgeGrowth(edge: Edge, polygon: Polygon, otherNodeStrategies: List[OtherNodeStrategy]): Either[GrowthLeft, List[Edge]] =
      tiling.perimeter.isOrientedAt(edge) match
        case None                          => Left(tiling.graphEdges, _.addToNonPerimeterEdgeErrMsg(edge))
        case Some(edgeIsPerimeterOriented) =>
          val firstSecond: (Node, Node) =
            if edgeIsPerimeterOriented then edge.pair else edge.pair.swap
          val (from, node): (Node, Node) =
            edgeImplementingStrategy(firstSecond, otherNodeStrategies)
          edgesFromPerimeterNodeGrowth(node, polygon, List(FIXED(from)))

    private def edgesToTilingAndEdges(f: (Tiling, List[Edge]) => Tiling): Either[GrowthLeft, List[Edge]] => Either[GrowthLeft, (Tiling, List[Edge])] =
      _.map(newEdges => (f(tiling, newEdges), newEdges))

    // if forming a full vertex, it must be grown starting from the minimum exterior angles
    private def loopGrowPerimeterNodeByFillingVertex(node: Node,
                                                     vertex: Vertex,
                                                     f: (Tiling, List[Edge]) => Tiling,
                                                     otherNodeStrategy: OtherNodeStrategy): Either[GrowthLeft, Tiling] =
      vertex.toPolygons.toList match
        case Nil => Right(tiling)
        case h :: polygons =>
          val (before, after): (Node, Node) =
            tiling.perimeter.beforeAfterO(node).get
          val start: Node =
            List(before, after).min(tiling.MinAngleNodeOrdering)
          val from: Node =
            (otherNodeStrategy: @unchecked) match
              case FIXED(from) => from
          val startFromBefore: Boolean =
            from == before
          val isReversed: Boolean =
            if startFromBefore then start == after else start == before
          val (head, tail): (Polygon, Vector[Polygon]) =
            if isReversed then (vertex.toPolygons.last, vertex.toPolygons.init.reverse) else (h, polygons.toVector)
          tiling.edgesToTilingAndEdges(f)(tiling.edgesFromPerimeterNodeGrowth(node, head, List(FIXED(start))))
            .flatMap((t, edges) =>
              edges.find(_.containsNode(node)) match
                case Some(edge) => t.loopGrowPerimeterNodeByFillingVertex(node, Vertex(tail), f, FIXED(edge.otherNode(node).get))
                case None       => Right(t)
            )

    /** Generic method to try to build a new `Tiling` with an additional vertex adjacent to a perimeter node
     *
     * @param node                perimeter node
     * @param vertex              added vertex
     * @param f                   function transforming a tiling and additional edges in a new tiling,
     *                            usually received unsafe from the Tiling case class to skip validation
     * @param otherNodeStrategies method to choose the other adjacent perimeter node,
     *                            identifying the edge that the first polygon of the vertex is sharing
     * @return Either the tiling or a failure message
     */
    def genericGrowPerimeterNodeByVertex(node: Node,
                                         vertex: Vertex,
                                         f: (Tiling, List[Edge]) => Tiling,
                                         otherNodeStrategies: List[OtherNodeStrategy]): Either[GrowthLeft, Tiling] =
      if vertex.isFull then
        Left((tiling.graphEdges, _ => "Vertex to be added cannot be full"))
      else
        vertex.toPolygons.toList match
          case Nil => Right(tiling)
          case h :: polygons =>
            val nodePolygons: Vector[Polygon] =
              tiling.perimeterOrderedPolygons.getOrElse(node, Vector.empty)
            if FullVertex.maybe(vertex.toPolygons ++ nodePolygons).isRight then
              val (before, after): (Node, Node) =
                tiling.perimeter.beforeAfterO(node).get
              nodeImplementStrategy(otherNodeStrategies)(node).flatMap(
                startFromBefore => loopGrowPerimeterNodeByFillingVertex(node, vertex, f, FIXED(if startFromBefore then before else after))
              )
            else
              val first =
                edgesToTilingAndEdges(f)(edgesFromPerimeterNodeGrowth(node, h, otherNodeStrategies))
              val grown: Either[GrowthLeft, (Tiling, List[Edge])] =
                polygons.foldLeft(first)((either, polygon) =>
                  either.flatMap((t, edges) =>
                    val edge: Edge =
                      edges.find(_.containsNode(node)).get
                    t.edgesToTilingAndEdges(f)(t.edgesFromPerimeterNodeGrowth(node, polygon, List(FIXED(edge.otherNode(node).get))))
                  )
                )
              grown.map((t, _) => t)

    // common method for both growing by node or by edge
    private def genericGrowByPolygon[T](steps: Int,
                                        polygon: Polygon,
                                        f: Tiling => Vector[T],
                                        g: (Tiling, T, Polygon) => Either[String, Tiling]): Either[String, Tiling] =
      val start: Either[String, Tiling] =
        Right(tiling)
      (0 until steps).foldLeft(start)((maybeTiling, step) =>
        maybeTiling.flatMap(t =>
          f(t)
            .view
            .map(g(t, _, polygon))
            .find(_.isRight)
            .getOrElse(Left(t.noFillablePerimeterEdgesErrMsg(polygon, step)))
        )
      )

    /** Tries to grow a tiling sequentially adding the same polygon, according to a strategy to find a suitable node */
    def growByPolygon(steps: Int,
                      polygon: Polygon,
                      perimeterStrategies: List[PerimeterStrategy],
                      otherNodeStrategies: List[OtherNodeStrategy]): Either[String, Tiling] =
      genericGrowByPolygon(steps, polygon, _.sortedPerimeterNodes(perimeterStrategies *), _.maybeGrowNode(_, _, otherNodeStrategies *))

    /** Tries to grow a tiling sequentially adding the same polygon, according to a strategy to find a suitable edge */
    def growByPolygonOnPerimeterEdges(steps: Int,
                                      polygon: Polygon,
                                      perimeterStrategies: List[PerimeterStrategy],
                                      otherNodeStrategies: List[OtherNodeStrategy]): Either[String, Tiling] =
      genericGrowByPolygon(steps, polygon, _.sortedPerimeterEdges(perimeterStrategies *), _.maybeGrowEdge(_, _, otherNodeStrategies *))

    private def isFillableBy(fullVertex: FullVertex, node: Node, polygon: Polygon, otherNodeStrategies: List[OtherNodeStrategy]): Boolean =
      val startFromBefore: Boolean =
        nodeImplementStrategy(otherNodeStrategies)(node).toOption.get
      pathNodesAndEnd(node, polygon, startFromBefore)
        .map((pathNodes, end, _) =>
          val vertices: List[Vector[Polygon]] =
            List(pathNodes.head, end).map(tiling.perimeterOrderedPolygons)
          val orientedVertices: List[Vector[Polygon]] =
            if startFromBefore then
              List(vertices.head, vertices(1).reverse)
            else
              List(vertices.head.reverse, vertices(1))
          orientedVertices.forall(polygons =>
            Try(Vertex(polygon +: polygons)).toOption.exists(_.isContainedIn(fullVertex.vertex))
          )
        )
        .getOrElse(false)

    /** Tries to create a simpler tiling by removing a node */
    def maybeRemoveNode(node: Node): Either[String, Tiling] =
      val removed: List[Edge] =
        tiling.graphEdges.withoutNodes(List(node))
      val newEdges: List[Edge] =
        removed.map(edge =>
          Edge(edge.nodes.map({
            case hi if hi > node => Node(hi.toInt - 1)
            case lo              => lo
          }))
       )
      Tiling.maybe(newEdges)

  /** Tries to create a tiling by growing by the same polygon for given steps */
  def maybePolygonGrow(polygon: Polygon, steps: Int): Either[String, Tiling] =
    fromPolygon(polygon).growByPolygon(steps - 1, polygon, List(NARROWEST_ANGLE, LOWEST_ORDINAL), List(HIGHER_ORDINAL))
