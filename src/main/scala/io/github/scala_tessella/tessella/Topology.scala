package io.github.scala_tessella.tessella

import utility.Utils.*
import io.github.scala_tessella.ring_seq.RingSeq.{Index, slidingO}

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.math.Ordered.orderingToOrdered
import scala.util.Try

/** Methods for graph nodes and edges */
object Topology:

  /** Graph node */
  opaque type Node = Int

  /** Companion object for [[Node]] */
  object Node:

    /** Create a [[Node]] from an `Int` */
    def apply(ordinal: Int): Node =
      ordinal

  /** Used as a convenience origin in some methods, cannot be part of an [[Edge]] */
  private val zeroNode: Node =
    Node(0)

  /** Nodes ordered by ascending ordinal */
  object NodeOrdering extends Ordering[Node]:

    def compare(node1: Node, node2: Node): Int =
      node1 compare node2

  object NodeSeqOrdering extends Ordering[Seq[Node]]:

    def compare(seq1: Seq[Node], seq2: Seq[Node]): Int =
      seq1 compare seq2

  /** Nodes ordered according being before or after the set [[BeforeAfterOrdering.before]] node */
  class BeforeAfterOrdering(beforeNode: Node) extends Ordering[Node]:

    def compare(a: Node, b: Node): Int =
      if a.equals(beforeNode) then -1
      else if b.equals(beforeNode) then 1
      else 0

  extension (node: Node)

    /** @return the underlying `Int` */
    def toInt: Int =
      node

    @targetName("greater")
    def >(that: Node): Boolean =
      node > that

    @targetName("greaterEq")
    def >=(that: Node): Boolean =
      node >= that

    @targetName("lesser")
    def <(that: Node): Boolean =
      node < that

  extension (nodes: Vector[Node])

    private def toEdgesRaw(f: Vector[Node] => Iterator[Vector[Node]]): Iterator[Edge] =
      f(nodes).map(Edge(_))

    /** Converts sequentially into edges
     *
     *  @throws IllegalArgumentException if nodes are equal or with an ordinal lesser than 1
     */
    def toEdges: Iterator[Edge] =
      toEdgesRaw(_.sliding(2))

    /** Converts sequentially  into circular edges
     *
     * @throws IllegalArgumentException if nodes are equal or with an ordinal lesser than 1
     */
    def toEdgesO: Iterator[Edge] =
      toEdgesRaw(_.slidingO(2))

  /** Count of graph edges that are incident to a node */
  opaque type Degree = Int

  /** Companion object for [[Degree]] */
  object Degree:

    /** Create a [[Degree]] from an `Int` */
    def apply(i: Int): Degree = i

  extension (deg: Degree)

    /** @return the underlying `Int` */
    @targetName("degreeToInt")
    def toInt: Int =
      deg

  /** Filter for nodes connected only to another node */
  def isPendant: Degree => Boolean =
    _ == Degree(1)

  /** Filter for nodes connected only to two other nodes */
  def isThread: Degree => Boolean =
    _ == Degree(2)

  /** Filter for nodes connected only to three other nodes */
  def isFork: Degree => Boolean =
    _ == Degree(3)
  
  private def withDegree(f: Degree => Boolean): ((Node, Degree)) => Boolean =
    (_, degree) => f(degree)

  extension (mapped: Map[Node, Degree])

    /** Selects only the associations according to the given degree filter */
    def onlyWithDegree(f: Degree => Boolean): Map[Node, Degree] =
      mapped.filter(withDegree(f))

  /** Graph edge */
  opaque type Edge = (Node, Node)

  /** Companion object for [[Edge]] */
  object Edge:

    private def notPositiveVertexErrMsg(node1: Node, node2: Node): String =
      s"Nodes must be positive: found ${toStringFromNodes((node1, node2))}"

    private def loopErrMsg(node: Node): String =
      s"Loop edge not allowed: found ${toStringFromNodes((node, node))}"

    /** Creates an `Edge` with the lesser `Node` first
     *
     * @param nodes `Node` pair
     * @throws IllegalArgumentException if nodes are equal or with an ordinal lesser than 1
     * @example {{{Edge((Node(2), Node(1)) // 1--2}}}
     */
    def apply(nodes: (Node, Node)): Edge =
      nodes match
        case (n1, n2) if n1 < 1 || n2 < 1 => throw new IllegalArgumentException(notPositiveVertexErrMsg(n1, n2))
        case (n1, n2) if n1 == n2         => throw new IllegalArgumentException(loopErrMsg(n1))
        case (n1, n2) if n1 < n2          => nodes
        case (n1, n2)                     => (n2, n1)

    /** Creates an `Edge` from two nodes */
    def apply(node1: Node, node2: Node): Edge =
      apply((node1, node2))

    /** Creates an `Edge` from the fist two nodes of a sequence */
    def apply(nodes: Seq[Node]): Edge =
      apply(nodes.toCouple)

    /** Creates a stringified edge from `Node` pair */
    def toStringFromNodes(nodes: (Node, Node)): String =
      s"${nodes._1}--${nodes._2}"

  private object EdgeLesserOrdering extends Ordering[Edge]:

    def compare(edge1: Edge, edge2: Edge): Int =
      edge1.lesserNode compare edge2.lesserNode

  private object EdgeGreaterOrdering extends Ordering[Edge]:

    def compare(edge1: Edge, edge2: Edge): Int =
      edge1.greaterNode compare edge2.greaterNode

  /** Edges ordered first by lesser node, then by greater node */
  object EdgeOrdering extends Ordering[Edge]:

    def compare(edge1: Edge, edge2: Edge): Int =
      EdgeLesserOrdering.orElse(EdgeGreaterOrdering).compare(edge1, edge2)

  /** Edges ordered by size */
  object EdgesSizeOrdering extends Ordering[List[Edge]]:

    def compare(a: List[Edge], b: List[Edge]): Int =
      a.sizeCompare(b)

  /** Edges ordered by nodes size */
  object EdgesNodesSizeOrdering extends Ordering[List[Edge]]:
    
    def compare(a: List[Edge], b: List[Edge]): Int =
      a.nodes.sizeCompare(b.nodes)

  /** Edges sets ordered by grouped size */
  object EdgesSizeMapOrdering extends Ordering[List[List[Edge]]]:

    def compare(a: List[List[Edge]], b: List[List[Edge]]): Int =
      if a.map(_.size).groupBySize == b.map(_.size).groupBySize then 0 else -1

  extension (edge: Edge)

    /** Edge node with the lower ordinal */
    def lesserNode: Node =
      edge._1

    /** Edge node with the higher ordinal */
    def greaterNode: Node =
      edge._2

    /** Edge nodes as a couple */
    def pair: (Node, Node) =
      (lesserNode, greaterNode)

    /** Edge as a string */
    def stringify: String =
      Edge.toStringFromNodes(pair)

    /** Edge nodes as a list */
    def nodes: List[Node] =
      List(lesserNode, greaterNode)

    /** Finds other edge node to given */
    def otherNode(node: Node): Option[Node] =
      if lesserNode == node then
        Option(greaterNode)
      else if greaterNode == node then
        Option(lesserNode)
      else
        None

    /** Checks if edge contains one node */
    def containsNode(node: Node): Boolean =
      otherNode(node).isDefined

    /** Checks if edge contains both nodes */
    private def hasBothNodesContainedIn(otherNodes: List[Node]): Boolean =
      nodes.diff(otherNodes).isEmpty

    private def hasAtLeastOneNodeContainedIn(otherNodes: List[Node]): Boolean =
      otherNodes.exists(containsNode)

  extension (ordinal: Int)

    /** Joins two nodes into an `Edge` */
    @targetName("edgeLigature")
    def --(other: Int): Edge =
      Edge((Node(ordinal), Node(other)))

  extension (edges: List[Edge])

    /** toString equivalent */
    def stringify: String =
      edges.sorted(EdgeOrdering).map(_.stringify).mkString(", ")

    /** Builds an adjacency map (node -> set of adjacent nodes) from a list of edges. */
    def adjacencyMap: Map[Node, Set[Node]] =
      val builder =
        mutable.Map[Node, mutable.Set[Node]]()
      edges.foreach { edge =>
        builder.getOrElseUpdate(edge.lesserNode, mutable.Set.empty) += edge.greaterNode
        builder.getOrElseUpdate(edge.greaterNode, mutable.Set.empty) += edge.lesserNode
      }
      builder.map { case (k, v) => k -> v.toSet }.toMap

    private def rawNodes: List[Node] =
      edges.flatMap(_.nodes)

    /** Distinct nodes */
    def nodes: List[Node] =
      rawNodes.distinct

    /** Degree of the give node */
    def degree(node: Node): Degree =
      Degree(edges.count(_.containsNode(node)))

    /** @return association of nodes and degrees */
    def allDegrees: Map[Node, Degree] =
      rawNodes.groupBySize.mapValues2(Degree(_))
      //adjacencyMap.mapValues2(nodes => Degree(nodes.size))

    private def nodesWithDegree(f: Degree => Boolean): List[Node] =
      allDegrees.onlyWithDegree(f).keys.toList

    /** Nodes connected to just one other node. */
    def pendantNodes: List[Node] =
      nodesWithDegree(isPendant)

    /** Nodes connected to just two other nodes. */
    def threadNodes: List[Node] =
      nodesWithDegree(isThread)

    /** Nodes adjacent to given nodes */
    def adjacentTo(node: Node): List[Node] =
      edges.filter(_.containsNode(node)).nodes.filterNot(_ == node)

    /** Filters edges where both their nodes are listed */
    def withOnlyNodes(nodes: List[Node]): List[Edge] =
      edges.filter(_.hasBothNodesContainedIn(nodes))

    /** Filters edges where no more than one of their nodes are listed */
    def withoutOnlyNodes(nodes: List[Node]): List[Edge] =
      edges.filterNot(_.hasBothNodesContainedIn(nodes))

    /** Filters edges where at least one of their nodes are listed */
    def withNodes(nodes: List[Node]): List[Edge] =
      edges.filter(_.hasAtLeastOneNodeContainedIn(nodes))

    /** Filters edges where none of their nodes are listed */
    def withoutNodes(nodes: List[Node]): List[Edge] =
      edges.filterNot(_.hasAtLeastOneNodeContainedIn(nodes))

    /** Renumbers all nodes of the edges in a continuous sequence of ordinals starting at a given node.
     *
     * @param node the lowest ordinal
     * @example {{{List(5--7, 7--9).compactStartingAt(Node(2)) // List(2--3, 3--4)}}}
     **/
    def compactStartingAt(node: Node = Node(1)): List[Edge] =
      val positions: Map[Node, Index] =
        (zeroNode :: edges.nodes.sorted(NodeOrdering)).zipWithIndex.tail.toMap
      val gap: Int =
        node - 1
      edges.map(edge => Edge(
        Node(positions(edge.lesserNode) + gap),
        Node(positions(edge.greaterNode) + gap)
      ))

    /** Renumbers all nodes of the edges in a continuous sequence of ordinals starting at 1.
     *
     * @note such a sequence is a necessary (but not sufficient) condition for the validity of a `Tiling`
     * @example {{{List(5--7, 7--9).compact // List(1--2, 2--3)}}}
     **/
    def compact: List[Edge] =
      compactStartingAt()

    /** Rearranges in a continuous sequence of ordinals the nodes after a given node.
     *
     * @param node the ordinal at the start of the rearranged sequence
     * @example {{{List(1--3, 3--9).compactAfter(Node(3)) // List(1--3, 3--4)}}}
     **/
    def compactAfter(node: Node): List[Edge] =
      val next: Int = node + 1
      edges.nodes.filter(_ >= next).minOption(NodeOrdering) match
        case Some(min) if min != next =>
          val gap: Int = min - next
          edges.map(edge => Edge(edge.nodes.map({
            case n if n >= next => Node(n - gap)
            case n              => n
          })))
        case _ => edges

    /** Breadth-first search.
     *
     * @param root starting node
     * @param target optional target node where to stop the search
     * @return if a target is given, a sequence of nodes forming a connected path from root;
     *         otherwise, all the nodes connected to root
     */
    private def bfs(root: Node, target: Option[Node] = None): List[Node] =
      if !nodes.contains(root) then
        Nil
      else
        val positions: Map[Node, Index] =
          nodes.zipWithIndex.toMap
        val parents =
          new Array[Node](nodes.length)

        /** Builds the path through parents from target to root */
        @tailrec
        def loop(path: List[Node]): List[Node] =
          path.head match
            case node if node == root => path
            case node                 => loop(parents(positions(node)) :: path)

        val queue: mutable.Queue[Node] =
          mutable.Queue.empty
        val visited =
          new Array[Boolean](nodes.length)

        /** Marks position as visited and enqueue node */
        def markAndEnqueue(node: Node, position: Index, parent: Node): Unit =
          visited(position) = true
          if target.isDefined then
            parents(position) = parent
          target match
            case Some(stop) if node == stop => queue.empty
            case _                          => queue.enqueue(node)

        markAndEnqueue(root, positions(root), zeroNode)
        while queue.nonEmpty do
          val parent: Node =
            queue.dequeue()
          for
            adjacent <- adjacentTo(parent)
            position = positions(adjacent)
            if !visited(position)
          do
            markAndEnqueue(adjacent, position, parent)
        val found: List[Node] =
          visited.indices.filter(visited).toList.map(nodes.toArray)
        target match
          case Some(stop) if !found.contains(stop) => Nil
          case Some(stop)                          => loop(List(stop))
          case None                                => found

    /** Breadth-first search.
     * It seems a faster option in the JHM benchamrks
     * @see bench/src/test/scala/io/github/scala_tessella/tessella/TopologyBenchmarkWithBFS.scala
     * when searching whole tilings.
     * It remains to be seen the real usage in Tessella, where maybe it is more often used on smaller portions.
     */
    private def bfsFast(root: Node, target: Option[Node] = None): List[Node] =
      // Compute distinct nodes and their positions once.
      val distinctNodes: Vector[Node] = 
        edges.flatMap(_.nodes).distinct.toVector

      // Initial check: if there are no nodes or root is not in the graph.
      if distinctNodes.isEmpty || !distinctNodes.contains(root) then
        Nil
      else
        val positions: Map[Node, Index] = distinctNodes.zipWithIndex.toMap

        // Build an adjacency list for efficient neighbor lookup.
        val adj = mutable.Map[Node, mutable.ListBuffer[Node]]()
        for edge <- edges do
          val (n1, n2) = edge // Edge is an opaque type (Node, Node)
          adj.getOrElseUpdate(n1, mutable.ListBuffer.empty) += n2
          adj.getOrElseUpdate(n2, mutable.ListBuffer.empty) += n1

        val parents = 
          new Array[Node](distinctNodes.length)

        /** Builds the path through parents from target to root */
        @tailrec
        def loop(path: List[Node]): List[Node] =
          path.head match
            case node if node == root => path
            case node => loop(parents(positions(node)) :: path)

        val queue: mutable.Queue[Node] = mutable.Queue.empty
        val visited = new Array[Boolean](distinctNodes.length)

        /** Marks position as visited and enqueue node */
        def markAndEnqueue(node: Node, position: Index, parentNode: Node): Unit =
          visited(position) = true
          if target.isDefined then
            parents(position) = parentNode
          target match
            // Early exit: If target is found, clear the queue to stop BFS.
            case Some(stop) if node == stop => queue.clear()
            case _                          => queue.enqueue(node)

        // Start BFS from the root node.
        val rootPosition = 
          positions(root) // root is confirmed to be in distinctNodes, so in positions
        markAndEnqueue(root, rootPosition, zeroNode)

        while queue.nonEmpty do
          val currentParentNode: Node = queue.dequeue()
          // Iterate over neighbors using the adjacency list.
          adj.get(currentParentNode).foreach { neighbors =>
            for
              adjacent <- neighbors
              // Ensure adjacent node is in positions map (should be true if graph is consistent)
              // and retrieve its position.
              localPosition <- positions.get(adjacent)
              if !visited(localPosition)
            do
              markAndEnqueue(adjacent, localPosition, currentParentNode)
          }

        // Optimized result construction.
        target match
          case Some(stopNode) =>
            // Check if the target node was visited.
            positions.get(stopNode).filter(visited(_)) match
              case Some(_) => loop(List(stopNode)) // Target reached, reconstruct path.
              case None    => Nil                  // Target not reachable.
          case None =>
            // No target, return all visited nodes.
            // Use the pre-computed distinctNodes Vector for mapping indices to Nodes.
            visited.indices.filter(visited(_)).map(distinctNodes(_)).toList

    /** The shortest path from a node to another one */
    def shortestPath(from: Node, to: Node): Vector[Node] =
      edges.bfs(from, Option(to)).toVector

    /** The distance (in numbers of nodes) from a node to another one */
    def distance(from: Node, to: Node): Int =
      shortestPath(from, to).size - 1

    /** The shortest path from a node to another one, avoiding the given nodes */
    def shortestPath(from: Node, to: Node, avoiding: List[Node]): Vector[Node] =
      edges.withoutNodes(avoiding).shortestPath(from, to)

    /** The distance (in numbers of nodes) from a node to another one, avoiding the given nodes */
    def distance(from: Node, to: Node, avoiding: List[Node]): Int =
      shortestPath(from, to, avoiding).size - 1

    /** All the nodes connected to a given node */
    def nodesConnectedTo(node: Node): List[Node] =
      edges.bfs(node)

    /** Finds the second lowest edge according to [[EdgeOrdering]] */
    def secondMinOption: Option[Edge] =
      Try(edges.sorted(EdgeOrdering)(1)).toOption
    
    /** Finds a circular path from a starter edge traversing all edges.
     *
     * @param starter method to choose the starting edge
     * @return an optional sequence of nodes forming a circular path
     */
    def maybeCircularNodes(starter: List[Edge] => Option[Edge]): Option[Vector[Node]] =
      for
        edge <- starter(edges)
        path <- Option(edges.filterNot(_ == edge).shortestPath(edge.lesserNode, edge.greaterNode))
        if edges.sizeCompare(path) == 0
      yield
        path

    /** Finds a path traversing all edges */
    def maybePathNodes: Option[Vector[Node]] =
      pendantNodes match
        case from :: to :: Nil => shortestPath(from, to) match
          case path if nodes.sizeCompare(path) == 0 => Option(path)
          case _                                    => None
        case _ => None

    def areContinuous: Boolean =
      edges.size < 2 || maybePathNodes.isDefined

    /** All sets of disconnected nodes. */
    def disconnectedNodes: List[List[Node]] =

      @tailrec
      def loop(remaining: List[Edge], acc: List[List[Node]]): List[List[Node]] =
        remaining.nodes match
          case Nil => acc
          case root :: _ =>
            val connected: List[Node] =
              remaining.bfs(root)
            loop(remaining.withoutNodes(connected), connected :: acc)

      loop(edges, Nil)

    /** Checks if edges are all connected */
    def areConnected: Boolean =
      disconnectedNodes.sizeIs == 1

    /** All sets of disconnected edges. */
    def disconnected: List[List[Edge]] =
      disconnectedNodes.map(withOnlyNodes)

    /** Computes all distinct pairs of nodes from the given polygon path in a tiling
     *  along with their minimum distance on the cycle.
     *
     * @param polygonNodes a vector containing ordered nodes in a polygon cycle
     */
    private def nodePairsWithDistance(polygonNodes: Vector[Node]): IndexedSeq[(Node, Node, Int)] =
      val sides = polygonNodes.length
      for
        i <- 0 until sides
        j <- (i + 1) until sides // Ensures each pair is unique and j > i
        diff = j - i
        distCycle = Math.min(diff, sides - diff)
      yield (polygonNodes(i), polygonNodes(j), distCycle)

    /** Checks if a polygon path in a tiling is minimal, with no internal paths among its nodes
     * 
     * @param polygonNodes the polygon path, unchecked
     */
    def isPolygonMinimal(polygonNodes: Vector[Node]): Boolean =
      nodePairsWithDistance(polygonNodes).forall { case (node1, node2, distCycle) =>
        // The shortest path distance between node1 and node2 in the overall graph.
        val distGraph = edges.distance(node1, node2)
        // If distGraph is shorter than distCycle, this cycle is not a minimal face.
        distGraph >= distCycle
      }

    def findPolygonInternalPath(polygonNodes: Vector[Node]): Option[(Node, Node)] =
      nodePairsWithDistance(polygonNodes).find { case (node1, node2, distCycle) =>
        val distGraph = edges.distance(node1, node2)
        distGraph < distCycle
      } map((node1, node2, _) => (node1, node2))
