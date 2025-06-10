package io.github.scala_tessella.tessella

import TilingCoordinates.*
import Topology.{Degree, Edge, EdgesNodesSizeOrdering, EdgesSizeOrdering, Node, NodeOrdering, NodeSeqOrdering, isFork, isThread}
import utility.Utils.{filterUnique, toCouple}
import utility.UtilsOption.firstDefined
import io.github.scala_tessella.ring_seq.RingSeq.{Index, applyO, reflectAt, slidingO, startAt}

import scala.annotation.tailrec
import scala.collection.{View, mutable}
import scala.util.Try

/** Undirected graph with gonality >= 1 (no nodes outside the edges)
 *
 * @param edges the graph edges
 */
class Graph(edges: List[Edge]):

  /** Path of adjacent graph nodes */
  opaque type Path = Vector[Node]

  /** Companion object for [[Path]] */
  object Path:

    /** Creates a [[Path]] faster without validity checks. */
    def unsafe(nodes: Vector[Node]): Path =
      nodes

    private def notBelongingErrMsg(node: Node): String =
      s"Invalid path: node $node doesn't belong to graph"

    private def notConnectedErrMsg(node1: Node, node2: Node): String =
      s"Invalid path: nodes $node1 and $node2 are not connected"

    private def checkOnly(node: Node): Vector[Node] =
      edges.find(_.containsNode(node)) match
        case Some(_) => Vector(node)
        case None    => throw new IllegalArgumentException(notBelongingErrMsg(node))

    private def checkMany(nodes: Vector[Node], f: Vector[Node] => Iterator[Vector[Node]]): Vector[Node] =
      f(nodes).find(nodesPair => !edges.contains(Edge(nodesPair))) match
        case None    => nodes
        case Some(e) =>
          if !graphNodes.contains(e(0)) then
            throw new IllegalArgumentException(notBelongingErrMsg(e(0)))
          else if !graphNodes.contains(e(1)) then
            throw new IllegalArgumentException(notBelongingErrMsg(e(1)))
          else
            throw new IllegalArgumentException(notConnectedErrMsg(e(0), e(1)))

    /** Creates a sequence of adjacent nodes ordered from start to end according to a function.
     *
     * @throws IllegalArgumentException if sequence don't represent a path in the [[Graph]]
     */
    def check(nodes: Vector[Node], f: Vector[Node] => Iterator[Vector[Node]]): Vector[Node] =
      if nodes.sizeIs == 1 then
        checkOnly(nodes.head)
      else
        checkMany(nodes, f)

    /** Creates a [[Path]] from a sequence of adjacent nodes ordered from start to end.
     *
     * @throws IllegalArgumentException if sequence don't represent a path in the [[Graph]]
     */
    def apply(nodes: Vector[Node]): Path =
      check(nodes, _.sliding(2))

    /** Creates a [[Path]] from a sequence of unordered adjacent edges.
     *
     * @return `None` if sequence don't represent a path in the [[Graph]], else an `Option`
     */
    def maybeSimpleFrom(edgesSubset: List[Edge]): Option[Path] =
      edgesSubset.maybePathNodes.map(Path(_))

    /** Creates a [[Path]] from a sequence of unordered adjacent edges.
     *
     * @return empty if sequence don't represent a path in the [[Graph]]
     */
    def simpleFrom(edgesSubset: List[Edge]): Path =
      maybeSimpleFrom(edgesSubset).getOrElse(Vector.empty)

  extension (nodes: Path)

    /** @return the underlying sequence of ordered adjacent nodes */
    def toNodes: Vector[Node] =
      nodes

    /** @return the underlying sequence of ordered adjacent edges */
    def toPathEdges: Iterator[Edge] =
      toNodes.toEdges

  /** Circular path of adjacent graph nodes */
  opaque type RingPath = Vector[Node]

  /** Companion object for [[RingPath]] */
  object RingPath:

    /** Creates a [[RingPath]] faster without validity checks. */
    def unsafe(nodes: Vector[Node]): RingPath =
      nodes

    /** Creates a [[RingPath]] from a sequence of adjacent nodes ordered from start to end.
     *
     * @throws IllegalArgumentException if sequence don't represent a circular path in the [[Graph]]
     */
    def apply(nodes: Vector[Node]): RingPath =
      Path.check(nodes, _.slidingO(2))

    private def rawMaybeSimpleFromEdges(edgesSubset: List[Edge], starter: List[Edge] => Option[Edge]): Option[RingPath] =
      edgesSubset.maybeCircularNodes(starter)
        .flatMap(nodes => Try(apply(nodes)).toOption)
        .find(_.sizeCompare(edgesSubset.size) == 0)

    /** Creates a [[RingPath]] from a sequence of unordered adjacent edges,
     * where first and last node of the path belong to the first edge of the sequence.
     *
     * @return `None` if sequence don't represent a circular path in the [[Graph]], else an `Option`
     */
    def maybeSimpleFromEdges(edgesSubset: List[Edge]): Option[RingPath] =
      rawMaybeSimpleFromEdges(edgesSubset, _.headOption)

    /** Creates a [[RingPath]] from a sequence of unordered adjacent edges,
     * where first and last node of the path belong to the second edge sorted by min ordinal nodes.
     *
     * @return `None` if sequence don't represent a circular path in the [[Graph]], else an `Option`
     */
    def maybeSimpleFromEdges2(edgesSubset: List[Edge]): Option[RingPath] =
      rawMaybeSimpleFromEdges(edgesSubset, _.secondMinOption)

    /** Creates a [[RingPath]] from a sequence of unordered adjacent edges,
     * where first and last node of the path belong to the first edge of the sequence.
     *
     * @return empty if sequence don't represent a circular path in the [[Graph]]
     */
    def simpleFromEdges(edgesSubset: List[Edge]): RingPath =
      maybeSimpleFromEdges(edgesSubset).getOrElse(Vector.empty)

  extension (nodes: RingPath)

    /** @return the underlying sequence of ordered adjacent nodes */
    def toRingNodes: Vector[Node] =
      nodes

    /** @return the underlying sequence of ordered adjacent edges */
    def toRingEdges: Iterator[Edge] =
      toRingNodes.toEdgesO

    /** Checks if each node is contained just once */
    def isSimple: Boolean =
      nodes.distinct.sizeCompare(nodes) == 0

    private def checkDefinedO[T](node: Node, f: Index => Option[T]): Option[T] =
      nodes.indexOf(node) match
        case -1    => None
        case index => f(index)

    private def nodeCloseToO(node: Node, step: Int): Option[Node] =
      checkDefinedO(node, i => Option(nodes.applyO(i + step)))

    /** Finds the node after the given node */
    def afterO(node: Node): Option[Node] =
      nodeCloseToO(node, 1)

    /** Finds the node before the given node */
    def beforeO(node: Node): Option[Node] =
      nodeCloseToO(node, -1)

    /** Finds the nodes before and after the given node */
    def beforeAfterO(node: Node): Option[(Node, Node)] =
      checkDefinedO(node, i => Option((nodes.applyO(i - 1), nodes.applyO(i + 1))))

    /** Finds the circular path rotated to start with the given node */
    def startAtNodeO(node: Node): Option[RingPath] =
      checkDefinedO(node, i => Option(RingPath(nodes.startAt(i))))

    /** Finds if edge nodes has the same orientation of the circular path */
    def isOrientedAt(edge: Edge): Option[Boolean] =
      if !toRingEdges.contains(edge) then None
      else Option(beforeO(edge.greaterNode).get.equals(edge.lesserNode))

  extension (paths: List[RingPath])

    def toPerimeterEdges: List[Edge] =
      paths.flatMap(_.toRingEdges).filterUnique.toList

    /** Orient rings sharing an edge, starting from the orientation of the last ring
     *
     * @note to have the same orientation, two rings sharing the same edge nodes must
     *       contain them one reversed respect to the other
     * @example {{{
     *          val g = Graph(1--2, 1--3, 2--3, 1--4, 2--4)
     *          val r123 = g.RingPath(Vector(Node(1), Node(2), Node(3)))
     *          val r124 = g.RingPath(Vector(Node(1), Node(2), Node(4)))
     *          List(r123, r124).oriented // List(Vector(3, 2, 1), Vector(1, 2, 4)) }}}
     */
    def oriented: List[RingPath] =

      /**
       * @param remaining unoriented
       * @param acc oriented
       * @param size size of unoriented
       * @param count count of unoriented without shared edge, if larger than size skip orientation
       */
      @tailrec
      def loop(remaining: List[RingPath], acc: List[RingPath], size: Int, count: Int = 0): List[RingPath] =
        remaining match
          case Nil => acc
          case h :: t =>
            val straight: List[List[Node]] =
              h.toRingNodes.toList.slidingO(2).toList
            val inverted: List[List[Node]] =
              straight.map(_.reverse)
            acc
              .find(_.toList.slidingO(2).exists(pair => straight.contains(pair) || inverted.contains(pair)))
              .map(_.toList.slidingO(2).exists(straight.contains)) match
              case Some(true)            => loop(t, h.reverse :: acc, size - 1)
              case Some(false)           => loop(t, h :: acc, size - 1)
              case None if count <= size => loop(t :+ h, acc, size, count + 1)
              case _                     => loop(t, h :: acc, size - 1)

      paths.reverse match
        case h :: t => loop(t, List(h), t.size)
        case Nil    => Nil

  override def toString: String =
    s"Graph(${edges.stringify})"

  /** All the graph edges */
  val graphEdges: List[Edge] =
    edges

  /** All the graph nodes */
  lazy val graphNodes: List[Node] =
    edges.nodes

  private lazy val mappedDegrees: Map[Node, Degree] =
    edges.allDegrees

  /** Comparison by edges size and then nodes size */
  def compare(that: Graph): Int =
    EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(this.graphEdges, that.graphEdges)

  /** Sequence of nodes that are not in a continuous sequence of ordinals starting at 1 */
  def diffFromCompacted: List[Node] =
    graphNodes.diff(List.range(1, graphNodes.length + 1).map(Node(_)))

  /** Checks if nodes are in a continuous sequence of ordinals starting at 1 */
  def isCompacted: Boolean =
    diffFromCompacted.isEmpty

  /** Renumbers nodes to a continuous sequence of ordinals starting at 1 */
  def compact: Graph =
    Graph(edges.compact)

  /** Checks if all nodes are connected
   *
   * @see https://en.wikipedia.org/wiki/Connectivity_(graph_theory)#Connected_vertices_and_graphs
   */
  def isConnected: Boolean =
    edges.disconnectedNodes.sizeIs < 2

  /** Sequence of disconnected graphs found in the graph */
  def disconnectedGraphs: List[Graph] =
    edges.disconnected.map(Graph(_))

  private type RemainingPartition = (List[Edge], List[Vector[Node]])

  private def mappedForkCombinedEdges: View[(Node, List[Edge])] =
    mappedDegrees
      .view
      .filter((_, degree) => isFork(degree))
      .map((node, _) => node -> edges.adjacentTo(node).combinations(2).toList.map(Edge(_)))

  /** Finds and extracts two perimeter triangles
   *
   * @return if successful, the remaining edges and the unoriented triangles
   */
  private def extractPerimeterCoupledTriangles: Option[RemainingPartition] =

    def sidesFront(adjacentEdges: List[Edge]): (List[Node], Node) =
      val sides: List[Node] =
        adjacentEdges.diff(edges).head.nodes
      val front: Node =
        adjacentEdges.nodes.diff(sides).head
      (sides, front)

    def areConnectedElsewhere(node: Node, adjacentEdges: List[Edge]): Boolean =
      val (sides, front): (List[Node], Node) =
        sidesFront(adjacentEdges)
      edges.shortestPath(sides.head, sides.last, List(node, front)).nonEmpty

    mappedForkCombinedEdges
      .find((node, adjacentEdges) => adjacentEdges.intersect(edges).size == 2 && areConnectedElsewhere(node, adjacentEdges))
      .map((node, adjacentEdges) =>
        val (sides, front): (List[Node], Node) =
          sidesFront(adjacentEdges)
        val polygonsNodes: List[List[Node]] =
          sides.map(List(front, _, node))
        (edges.withoutNodes(List(node)), polygonsNodes.map(_.toVector))
      )

  /** Finds and extracts a perimeter polygon sharing at least two edges with the graph
   *
   * @return if successful, the remaining edges and the unoriented polygon
   */
  private def extractPerimeterPolygons: Option[RemainingPartition] =
    mappedDegrees.filter((_, degree) => isThread(degree)).keys.toList match
      case Nil => None
      case threadNodes =>
        val threadEdges: List[Edge] =
          edges.withOnlyNodes(threadNodes)
        if threadNodes.sizeCompare(edges) == 0 then
          val partialEdgeSets: List[List[Edge]] =
            threadEdges.disconnected
          Option(Nil, partialEdgeSets.map(set => RingPath.simpleFromEdges(set)))
        else
          val loneDisconnectedNodes: List[Node] =
            threadNodes.diff(threadEdges.nodes)
          (loneDisconnectedNodes.map(List(_)) ++ threadEdges.disconnectedNodes)
            .view
            .map(nodes => Path.simpleFrom(edges.withNodes(nodes)))
            .filter(_.nonEmpty)
            .map(external =>
              val shortest: List[Node] => Vector[Node] =
                nodes => edges.shortestPath(external.head, external.last, external.init.toList.tail ++ nodes)
              val internal: Vector[Node] =
                shortest(Nil).init.tail
              val alternative: Vector[Node] =
                shortest(internal.toList)
              (external, internal, alternative)
            )
            .find((_, _, alternative) => alternative.nonEmpty)
            .map((ext, internal, _) => (edges.withoutNodes(ext.init.toList.tail), List(ext.reverse ++ internal)))

  /** Finds and extracts a perimeter triangle
   *
   * @param isAdjacentToThread if true, safer, the perimeter triangle must be adjacent to a perimeter thread node
   * @return if successful, the remaining edges and the unoriented triangle
   */
  private def extractPerimeterStuckTriangle(isAdjacentToThread: Boolean = true): Option[RemainingPartition] =
    mappedForkCombinedEdges
      .filter((_, adjacentEdges) => adjacentEdges.count(edges.contains) == 1)
      .map((node, adjacentEdges) =>
        val sides: List[Node] =
          adjacentEdges.find(edges.contains).get.nodes
        val third: Node =
          adjacentEdges.nodes.diff(sides).head
        val front: Node =
          sides.minBy(edges.distance(_, third, List(node)))
        node -> (sides, third, front)
      )
      .find({ case (node, (sides, third, front)) =>
        (!isAdjacentToThread || isThread(mappedDegrees(third)))
          && edges.shortestPath(third, sides.filterNot(_ == front).head, List(node, front)).nonEmpty
      })
      .map({ case (node, (sides, _, front)) =>
        (edges.filterNot(_ == Edge(node, sides.filterNot(_ == front).head)), List((node :: sides).toVector))
      })

  private def extractPerimeterStuckTriangle2: Option[RemainingPartition] =
    mappedDegrees.filter((_, degree) => isFork(degree)).keys.toList
      .combinations(2)
      .map(nodes => (nodes, nodes.map(edges.adjacentTo)))
      .filter((nodes, adjacentNodes) => (nodes ++ adjacentNodes.flatten).distinct.size == 5)
      .map((nodes, adjacentNodes) => (Edge(nodes), adjacentNodes.head.intersect(adjacentNodes(1))))
      .find((edge, third) => edges.filterNot(_ == edge).shortestPath(edge.lesserNode, edge.greaterNode, third).nonEmpty)
      .map((edge, third) => (edges.filterNot(_ == edge), List(third.head +: edge.nodes.toVector)))

  /** Finds all the unoriented polygons composing the graph; it should always be defined if the graph is a [[Tiling]]
   *  The algorithm is a quite bizantine heuristic "pull", progressively peeling away perimeter polygons.
   *
   * @return a sequence of circular paths, each representing a polygon
   */
  def tilingUnorientedPolygons: Option[List[RingPath]] =

    @tailrec
    def loop(remaining: List[Edge], acc: List[RingPath]): Option[List[RingPath]] =
      if remaining.isEmpty then
        Option(acc)
      else
        List[Graph => Option[RemainingPartition]](
          _.extractPerimeterPolygons,
          _.extractPerimeterCoupledTriangles,
          _.extractPerimeterStuckTriangle(),
          _.extractPerimeterStuckTriangle2,
          _.extractPerimeterStuckTriangle(false)
        ).firstDefined(Graph(remaining)) match
          case Some((newEdges, addition)) => loop(newEdges, addition ++ acc)
          case None                       => None

    loop(edges, Nil)

  /** Returns all simple cycles up to maxLength (as node vectors, canonicalized and with direction normalized). */
  def findSimpleCycles(maxLength: Int): List[RingPath] =
    val adjacency: Map[Node, Set[Node]] = edges.adjacencyMap
    val result = mutable.Set[Vector[Node]]()
    val path = mutable.ArrayBuffer[Node]()
    val blocked = mutable.Set[Node]()

    /** Returns the lexicographically minimal rotation of a cycle or its reverse */
    def canonicalCycle(cycle: Vector[Node]): Vector[Node] =
      // Remove any repeated last(start) node
      val simple = if cycle.head == cycle.last then cycle.dropRight(1) else cycle
      val rotations = simple.indices.map(i => simple.drop(i) ++ simple.take(i))
      val reversed = simple.reverse
      val rotationsRev = reversed.indices.map(i => reversed.drop(i) ++ reversed.take(i))
      (rotations ++ rotationsRev).min(NodeSeqOrdering)

    def dfs(start: Node, current: Node, depth: Int): Unit =
      if depth > maxLength then return
      path.append(current)
      blocked.add(current)
      for (nbr <- adjacency.getOrElse(current, Set.empty))
        if nbr == start && path.length >= 3 then
          // Found a cycle
          val cycle = canonicalCycle(path.toVector)
          result.add(cycle)
        else if !blocked.contains(nbr) && nbr > start then // prevents duplicate cycles
          dfs(start, nbr, depth + 1)
      path.remove(path.length - 1)
      blocked.remove(current)

    val nodes = adjacency.keys.toIndexedSeq.sorted(NodeOrdering)
    for (i <- nodes.indices)
      val start = nodes(i)
      path.clear()
      blocked.clear()
      dfs(start, start, 1)

    result.toList

  /** An alternative to tilingUnorientedPolygons, based on a clearer "push" algorithm.
   *  The approach is more brute force and could be quite slow if the maxLenght is set to 42, better if 12
   * */
  def tilingUnorientedPolygonsAlt(maxLength: Int): List[Vector[Node]] =
    findSimpleCycles(maxLength).filter { cycleNodes =>
      val sides = cycleNodes.length
      if sides < 3 then
        false // Polygons must have at least 3 sides.
      else
        // Create all distinct pairs of nodes from the cycle, along with their min distance on the cycle.
        val nodePairsWithDistance = for {
          i <- 0 until sides
          j <- (i + 1) until sides // Ensures each pair is unique and j > i
          diff = j - i
          distCycle = Math.min(diff, sides - diff)
        } yield (cycleNodes(i), cycleNodes(j), distCycle)

        // A cycle is minimal if for ALL pairs of its nodes, the shortest path
        // in the graph is not shorter than the path along the cycle's perimeter.
        val isMinimal = nodePairsWithDistance.forall { case (node1, node2, distCycle) =>
          // Fetch the shortest path distance between node1 and node2 in the overall graph.
          val distGraph = edges.distance(node1, node2)
          // If distGraph is shorter than distCycle, this cycle is not a minimal face.
          distGraph >= distCycle
        }
        isMinimal
    }

  /** Finds all the oriented polygons composing the graph, it should always be defined if the graph is a [[Tiling]]
   *
   * @return a sequence of circular paths, each representing a polygon, all with the same orientation
   */
  lazy val tilingOrientedPolygons: Option[List[RingPath]] =
    tilingUnorientedPolygons.map(_.oriented)

  /** Finds the perimeter of the graph, it should always be defined if the graph is a [[Tiling]]
   *
   * @return a circular path, with the same orientation of the [[Graph.tilingOrientedPolygons]]
   */
  lazy val tilingOrientedPerimeter: Option[RingPath] =
    tilingOrientedPolygons
      .map(_.toPerimeterEdges)
      .flatMap(RingPath.maybeSimpleFromEdges2)
      .map(unorientedPerimeter =>
        val unorientedPerimeterEdge: (Node, Node) =
          unorientedPerimeter.take(2).toCouple
        val isStraight: Boolean =
          tilingOrientedPolygons.get.exists(_.toRingNodes.slidingO(2).map(_.toCouple).contains(unorientedPerimeterEdge))
        if isStraight then unorientedPerimeter else RingPath.unsafe(unorientedPerimeter.toRingNodes.reflectAt())
      )

  /** Tries converting into a valid [[Tiling]] */
  def toMaybeTiling: Either[String, Tiling] =
    Tiling.maybe(compact.graphEdges)

/** Companion object for [[Graph]] */
object Graph:

  /** Standard constructor */
  def apply(edges: List[Edge]): Graph =
    new Graph(edges)

  /** Convenience constructor */
  def apply(edges: Edge*): Graph =
    Graph(edges.toList)

  /** Creates a complete [[Graph]]
   *
   * @param size number of vertices
   * @see https://en.wikipedia.org/wiki/Complete_graph
   */
  def complete(size: Int): Graph =
    Graph((1 to size).combinations(2).map(nodes => Edge(nodes.map(Node(_)))).toList)
