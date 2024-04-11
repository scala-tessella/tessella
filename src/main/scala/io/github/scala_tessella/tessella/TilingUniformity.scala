package io.github.scala_tessella.tessella

import RegularPolygon.{Polygon, Vertex}
import Topology.*
import utility.Utils.*
import utility.UtilsOption.*

import io.github.scala_tessella.ring_seq.RingSeq.*

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering

/** Methods dealing with the uniformity of a tiling */
object TilingUniformity:

  /** Leaves of a tree of minor full vertices, each a different uniformity group */
  type VertexIndices = (Vertex, List[Index])

  extension (vertexIndices: VertexIndices)

    private def deconstruct: (Vector[Int], List[Int]) =
      vertexIndices match
        case (vertex, indices) => (vertex.toPolygons.map(_.toSides), indices)

  /** Leaves ordered first by ascending polygon sides, then by indices */
  private object VertexIndicesSeqOrdering extends Ordering[Seq[VertexIndices]]:

    def compare(a: Seq[VertexIndices], b: Seq[VertexIndices]): Int =
      a.map(deconstruct) compare b.map(deconstruct)

  extension [A](options: Options[A])

    def isRotationOrReflectionWhereDefined(other: Options[A]): Boolean =
      other.rotationsAndReflections.exists(options.equalsWhereDefined)

//    def findRotationOrReflectionWhereDefined(other: Options[A]): Option[Options[A]] =
//      other.rotationsAndReflections.find(options.equalsWhereDefined)

    private def filterRotationOrReflectionWhereDefined(other: Options[A]): Iterator[Options[A]] =
      other.rotationsAndReflections.filter(options.equalsWhereDefined)

    private def filterReflectionWhereDefined(other: Options[A]): Iterator[Options[A]] =
      other.reflections.filter(options.equalsWhereDefined)

  private type UniformTree = Map[List[Index], List[Node]]

  private type StripWithLoopFlag = (List[Int], Boolean)
  
  extension (tiling: Tiling)

    private def leavesFromLayer(matrixLayer: Map[Node, StripWithLoopFlag]): List[List[Node]] =
      val (loops, partials): (Map[Node, StripWithLoopFlag], Map[Node, StripWithLoopFlag]) =
        matrixLayer.partition({ case (_, (_, isLoop)) => isLoop })
      val withoutLoopInfo: List[(Node, StripWithLoopFlag)] => List[(Node, List[Int])] =
        _.map({ case (node, (sizes, _)) => (node, sizes) })
      val groupingLoops: List[List[(Node, List[Int])]] =
        withoutLoopInfo(loops.toList)
          .groupConnected({ case ((_, sizes1), (_, sizes2)) => sizes1.isRotationOrReflectionOf(sizes2)})
      val withExtra: Map[List[Int], List[Node]] =
        groupingLoops.map(w => (w.head._2, w.toMap.keys.toList)).toMap + (Nil -> Nil)
      val addingPartials: Map[List[Int], List[Node]] =
        withoutLoopInfo(partials.toList)
          .foldLeft(withExtra)({ case (acc, (partialNode, partialSizes)) =>
            acc.toList.filter((sizes, _) => partialSizes.size < sizes.size && sizes.reflections.exists(_.containsSliceO(partialSizes))) match
              case Nil                           => acc.updated(Nil, partialNode :: acc(Nil))
              case (onlySizes, onlyNodes) :: Nil => acc.updated(onlySizes, partialNode :: onlyNodes)
              case _                             => acc
          })
      addingPartials.values.toList.filter(_.nonEmpty)

    private def uniformNodesTree(matrix: Map[Node, List[StripWithLoopFlag]]): UniformTree =

      @tailrec
      def loop(reducedMatrix: Map[Node, List[StripWithLoopFlag]], acc: UniformTree): UniformTree =
        if reducedMatrix.isEmpty then
          acc
        else
          val innerLayer: Map[Node, StripWithLoopFlag] =
            reducedMatrix.map((node, values) => node -> values.head)
          val newAcc: UniformTree =
            acc.toList.flatMap((indexes, nodes) =>
              leavesFromLayer(innerLayer.filter((node, _) => nodes.contains(node))) match
                case many @ _ :: _ :: _ => many.indices.toList.map(index => (indexes :+ index, many(index)))
                case _                  => List((indexes, nodes))
            ).toMap
          val newReducedMatrix: Map[Node, List[StripWithLoopFlag]] =
            reducedMatrix.map((node, values) => node -> values.tail).filter((_, values) => values.nonEmpty)
          loop(newReducedMatrix, newAcc)

      loop(matrix, Map(Nil -> matrix.keys.toList))

    private def tailed(polygons: List[tiling.PolygonEdges]): List[tiling.PolygonEdges] =
      val noSharedPerimeterNodes: tiling.PolygonEdges => Boolean =
        polygonEdges => tiling.PolygonPath.unsafe(polygonEdges.nodes.toVector).hasNoSharedNodesWith(tiling.perimeter.toRingNodes)
      polygons.indexWhere(noSharedPerimeterNodes) match
        case -1 => Nil
        case indexStraight =>
          val oneSide = polygons.drop(indexStraight).reverse
          oneSide.indexWhere(noSharedPerimeterNodes) match
            case -1 => throw new Error("should not happen")
            case indexReverse => oneSide.drop(indexReverse)

    private def toOrderedPolygons(polygons: List[tiling.PolygonEdges], acc: List[List[tiling.PolygonEdges]]): (List[tiling.PolygonEdges], Boolean) =
      if acc.isEmpty then
        val startPerimeter: List[Edge] =
          tiling.unorderedPerimeterEdges(polygons)()
        val orderedEdges: List[Edge] =
          tiling.RingPath.simpleFromEdges(startPerimeter).toRingEdges.toList
        val isLoop: Boolean =
          Vertex(polygons.toVector.map(_.size).map(Polygon(_))).isFull
        (orderedEdges.map(edge => polygons.find(_.contains(edge)).get).distinct, isLoop)
      else
        val innerPerimeter: List[Edge] =
          acc.flatMap(_.flatten).filterUnique.toList
        val threadNodes: List[Node] =
          innerPerimeter.allDegrees.filter((_, degree) => isThread(degree)).keys.toList
        val disconnectedInnerPerimeters: List[List[Edge]] =
          innerPerimeter.withNodes(threadNodes).disconnected
        val largestInnerPerimeterChunk: List[Edge] =
          disconnectedInnerPerimeters.maxBy(_.size)
        val isLoop: Boolean =
          disconnectedInnerPerimeters.size == 1 && innerPerimeter.diff(polygons.flatten).isEmpty
        val all: List[tiling.PolygonEdges] =
          polygons.filter(tiling.hasSharedSidesWith(_)(largestInnerPerimeterChunk))
        val partialEdges: List[Edge] =
          all.flatten.disconnected.maxByOption(_.size).getOrElse(Nil)
        val partialPolygons: List[tiling.PolygonEdges] =
          polygons.filter(tiling.hasSharedSidesWith(_)(partialEdges))
        val partialPerimeter: List[Edge] =
          (tiling.unorderedPerimeterEdges(partialPolygons)() ++ innerPerimeter).filterNotUnique.toList
        val orderedEdges: List[Edge] =
          if isLoop then
            tiling.RingPath.simpleFromEdges(partialPerimeter).toRingEdges.toList
          else
            tiling.Path.simpleFrom(partialPerimeter).toPathEdges.toList
        val orderedPolygons: List[tiling.PolygonEdges] =
          orderedEdges.map(edge => partialPolygons.find(_.contains(edge)).get)
        val orderedTailedPolygons: List[tiling.PolygonEdges] =
          if isLoop then orderedPolygons else tailed(orderedPolygons)
        (orderedTailedPolygons, isLoop)

    private def outerOrderedPolygonsFrom(origins: List[Node], isStrict: Boolean = true): Map[Node, List[(List[tiling.PolygonEdges], Boolean)]] =
      tiling.outerFrom(origins, toOrderedPolygons, isStrict)

    /** Gets a sequence from origin to outer of polygons rings or single biggest partial portions of polygons ring */
    def outerOrderedPolygonsFromSingle(origin: Node, isStrict: Boolean = true): List[(List[tiling.PolygonEdges], Boolean)] =
      outerOrderedPolygonsFrom(List(origin), isStrict).values.head

    /** Association of node and sequence from node to outer of sizes of polygons rings or single biggest partial portions of sizes of polygons ring */
    def outerOrderedStripFrom(origins: List[Node], isStrict: Boolean = true): Map[Node, List[StripWithLoopFlag]] =
      tiling.outerOrderedPolygonsFrom(origins, isStrict)
        .map((node, value) => node -> value.map((polygons, isLoop) => (polygons.map(_.size), isLoop)))

    /** Gets a sequence from origin to outer of sizes of polygons rings or single biggest partial portions of sizes of polygons ring */
    def outerOrderedStripFromSingle(origin: Node, isStrict: Boolean = true): List[StripWithLoopFlag] =
      outerOrderedStripFrom(List(origin), isStrict).values.head

    /** Association of leaves of a tree of minor full vertices and nodes.
     *
     * @note while with gonality each nodes group belongs to a different vertex,
     *       with uniformity several nodes groups may belong to the same vertex
     */
    def groupUniforms: Map[VertexIndices, IndexedSeq[Node]] =
      tiling.groupGonals.flatMap((vertex, origins) =>
        val matrix: Map[Node, List[StripWithLoopFlag]] =
          tiling.outerOrderedStripFrom(origins.toList)
        if matrix.isEmpty then
          Map()
        else
          uniformNodesTree(matrix).map((indexes, nodes) => (vertex, indexes) -> nodes.toIndexedSeq)
      )

    /** Number of different node outer expansions in the tiling */
    def uniformity: Int =
      groupUniforms.size

    /** Expands the map searching for additional uniform nodes both by same polygons and same adjacent nodes */
    private def uniformityComplete(groupUniforms: Map[VertexIndices, IndexedSeq[Node]]): Map[Node, VertexIndices] =
      val nodeUniforms: Map[Node, VertexIndices] =
        groupUniforms.mapByValue
      val describedPolygonPaths: List[tiling.PolygonPath] =
        tiling.orientedPolygons.filter(_.toPolygonPathNodes.forall(nodeUniforms.contains))
      val polygonReferences: List[Vector[Option[VertexIndices]]] =
        describedPolygonPaths
          .groupBy(path => (
            path.toPolygonPathNodes.size,
            path.toPolygonPathNodes.map(nodeUniforms).rotationsAndReflections.toVector.min(VertexIndicesSeqOrdering)
          ))
          .keys.toList.map((_, polygonVertices) => polygonVertices.toOptionsVector)
      val adjacentReferences: Map[VertexIndices, Vector[Option[VertexIndices]]] =
        groupUniforms
          .mapValues2(_
            .find(node => tiling.nonPerimeterOrderedAdjacentNodes.get(node).exists(
              _.forall(n => tiling.nonPerimeterOrderedAdjacentNodes.isDefinedAt(n) && nodeUniforms.isDefinedAt(n)))
            )
            .map(node => tiling.nonPerimeterOrderedAdjacentNodes(node).map(nodeUniforms))
            .getOrElse(IndexedSeq.empty)
          )
          .filter((_, seq) => seq.nonEmpty)
          .mapValues2(_.toVector.toOptionsVector)
      val referencedIndices: List[VertexIndices] =
        adjacentReferences.keys.toList

      def loop(uni: Map[Node, VertexIndices], excluded: List[tiling.PolygonPath], limit: Int): Map[Node, VertexIndices] =
        val partialPolygons: List[tiling.PolygonPath] =
          tiling.orientedPolygons
            .filterNot(excluded.contains)
            .filterNot(_.toPolygonPathNodes.forall(uni.contains))

        @tailrec
        def loop2(gap: Int): Option[tiling.PolygonPath] =
          partialPolygons.find(_.toPolygonPathNodes.count(!uni.contains(_)) == gap) match
            case None if gap == 5 => None
            case None => loop2(gap + 1)
            case some => some

        loop2(1) match
          case Some(path) =>
            val nodes = path.toPolygonPathNodes
            val polygonVertices = nodes.map(uni.get)
            polygonReferences.filter(polygonVertices.isRotationOrReflectionWhereDefined) match
              case reference :: Nil =>
                polygonVertices.filterRotationOrReflectionWhereDefined(reference).toList.distinct match
                  case one :: Nil =>
                    val result: Map[Node, VertexIndices] =
                      nodes.indices.filter(index => polygonVertices(index).isEmpty).map(index => nodes(index) -> one(index).get).toMap
                    val nowIncluded: List[tiling.PolygonPath] =
                      result.keys.flatMap(node => tiling.orientedPolygons.filter(_.toPolygonPathNodes.contains(node))).toList.distinct
                    loop(uni ++ result, excluded.diff(nowIncluded), 0)
                  case _ => loop(uni, path :: excluded, limit)
              case _ => loop(uni, path :: excluded, limit)
          case None =>

            @tailrec
            def loop3(uni2: Map[Node, VertexIndices], excludedNodes: List[Node], limit: Int): Map[Node, VertexIndices] =
              val filteredUniforms: Map[Node, VertexIndices] =
                uni2
                  .filter((_, ref) => referencedIndices.contains(ref))
                  .filterNot((node, _) => excludedNodes.contains(node))

              @tailrec
              def loop4(gap: Int): Option[Node] =
                filteredUniforms
                  .find((node, _) =>
                    tiling.orderedAdjacentNodes.get(node).exists(_.count(!uni2.contains(_)) == gap)
                  ) match
                  case None if gap == 5 => None
                  case None => loop4(gap + 1)
                  case Some(node, _) => Some(node)

              loop4(1) match
                case Some(node) =>
                  val nodes = tiling.orderedAdjacentNodes(node)
                  val adjacentVertices = nodes.map(uni2.get)
                  val reference = adjacentReferences(uni2(node))
                  val filtered =
                    if tiling.perimeter.toRingNodes.contains(node) then
                      adjacentVertices.filterReflectionWhereDefined(reference)
                    else
                      adjacentVertices.filterRotationOrReflectionWhereDefined(reference)
                  filtered.toList.distinct match
                    case one :: Nil =>
                      val result: Map[Node, VertexIndices] =
                        nodes.indices.filter(index => adjacentVertices(index).isEmpty).map(index => nodes(index) -> one(index).get).toMap
                      val nowIncluded: List[Node] =
                        result.keys.flatMap(tiling.orderedAdjacentNodes.getOrElse(_, Nil)).toList.distinct
                      loop3(uni2 ++ result, excludedNodes.diff(nowIncluded), 0)
                    case _ => loop3(uni2, node :: excludedNodes, limit)
                case None =>
                  if limit == 2 then uni2
                  else loop(uni2, Nil, limit + 1)

            if limit == 2 then uni
            else loop3(uni, Nil, limit + 1)

      loop(nodeUniforms, Nil, 0)

    /** Expanded association of leaves of a tree of minor full vertices and nodes.
     * 
     * @note the uniformity value remains the same, but more nodes are found 
     */
    def groupUniformsComplete: Map[VertexIndices, IndexedSeq[Node]] =
      if uniformity == 1 then
        groupUniforms.mapValues2(_ => tiling.graphNodes.toIndexedSeq)
      else
        uniformityComplete(groupUniforms).groupByValues.mapValues2(_.toIndexedSeq)

    /** Transposition to better render the uniformity tree */
    def groupUniformsNestedComplete: Map[Vertex, List[(List[Index], IndexedSeq[Node])]] =
      tiling.groupUniformsComplete
        .groupBy({ case ((vertex, _), _) => vertex })
        .mapValues2(_.toList.map({ case ((_, treeIndices), nodes) => treeIndices -> nodes }))
