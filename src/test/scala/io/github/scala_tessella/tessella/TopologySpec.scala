package io.github.scala_tessella.tessella

import Topology.*
import Outliers.p4444_4by4_reticulate

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TopologySpec extends AnyFlatSpec with should.Matchers {

  "A Node" can "be created as an opaque type" in {
    Node(1).toString shouldBe
      "1"
  }

  def foo(node: Node) =
    true

  it must "be the only accepted param type" in {
    foo(Node(1)) shouldBe
      true
  }

  "An int" must "NOT be accepted as an equivalent param type" in {
    "foo(1)" shouldNot compile
  }

  "A Node" can "return its underlying value" in {
    Node(2).toInt shouldBe
      2
  }

  val n1: Node =
    Node(2)
  val n2: Node =
    Node(2)

  it can "be == to another node" in {
    (n1 == n2) shouldBe
      true
  }

  it can "be equal to another node" in {
    (n1 equals n2) shouldBe
      true
  }

  val m: Node =
    Node(3)

  it can "be != from another node" in {
    (n1 != m) shouldBe
      true
  }

  it can "be NOT equal to another node" in {
    (n1 equals m) shouldBe
      false
  }

  it can "NOT be directly compared to its underlying value" in {
    "n1 == 2" shouldNot compile
  }

  it can "be equal to its underlying value" in {
    (n1 equals 2) shouldBe
      true
  }

  val someNodes: List[Node] =
    List(1, 4, 3, 2, 2).map(Node(_))

  "A list of nodes" can "be sorted" in {
    someNodes.sorted(NodeOrdering) shouldBe
      List(1, 2, 2, 3, 4)
  }

  it can "be made distinct" in {
    someNodes.distinct shouldBe
      List(1, 4, 3, 2)
  }

  it can "be checked if a node is contained" in {
    someNodes.contains(Node(4)) shouldBe
      true
  }

  it can "be checked if an underlying value is contained" in {
    someNodes.contains(4) shouldBe
      true
  }

  it can "be converted into edges" in {
    someNodes.distinct.toVector.toEdges.toList shouldBe
      List(1--4, 4--3, 3--2)
  }

  it can "be NOT converted into edges when a loop is formed" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { someNodes.toVector.toEdges.toList }
    caught.getMessage shouldBe
      "Loop edge not allowed: found 2--2"
  }

  it can "be converted into circular edges" in {
    someNodes.distinct.toVector.toEdgesO.toList shouldBe
      List(1--4, 4--3, 3--2, 2--1)
  }

  it can "be NOT converted into circular edges when a loop is formed" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] {someNodes.toVector.toEdgesO.toList }
    caught.getMessage shouldBe
      "Loop edge not allowed: found 2--2"
  }

  val otherNodes: Seq[Node] =
    Seq(5, 4, 3, 6).map(Node(_))

  "Two lists of nodes" can "be intersected" in {
    someNodes.intersect(otherNodes) shouldBe
      List(4, 3)
  }

  they can "be diffed" in {
    someNodes.diff(otherNodes) shouldBe
      List(1, 2, 2)
  }

  "A Degree" can "be created as an opaque type" in {
    Degree(1).toString shouldBe
      "1"
  }

  it can "return its value" in {
    Degree(2).toInt shouldBe
      2
  }

  val anEdge: Edge =
    1--2

  "An Edge" can "be printed" in {
    anEdge.toString shouldBe
      "(1,2)"
  }

  it can "be pretty printed" in {
    anEdge.stringify shouldBe
      "1--2"
  }

  it can "have its two nodes listed" in {
    anEdge.nodes shouldEqual
      List(1, 2)
  }

  it can "be checked for containing its lesser node" in {
    anEdge.containsNode(Node(1)) shouldBe
      true
  }

  it can "be checked for containing its greater node" in {
    anEdge.containsNode(Node(2)) shouldBe
      true
  }

  it can "be checked for containing another node" in {
    anEdge.containsNode(Node(4)) shouldBe
      false
  }

  it can "be created with an infix operator" in {
    3--4 shouldEqual
      Edge((Node(3), Node(4)))
  }

  it must "NOT be a loop" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { 2--2 }
    caught.getMessage shouldBe
      "Loop edge not allowed: found 2--2"
  }

  it must "NOT have nodes smaller than 1" in {
    val caught: IllegalArgumentException =
      intercept[IllegalArgumentException] { 0--2 }
    caught.getMessage shouldBe
      "Nodes must be positive: found 0--2"
  }

  it must "be equal if nodes swapped" in {
    Edge((Node(3), Node(1))) shouldEqual
      Edge(Node(1), Node(3))
  }

  val inverted: Edge =
    3--1

  "An edge with smaller node as second param at creation" must "have as first node always the smaller of the two" in {
    inverted.nodes.head shouldEqual
      1
  }

  it can "find the lesser node alternatively" in {
    inverted.lesserNode shouldEqual
      1
  }

  it must "have as second node always the greater of the two" in {
    inverted.nodes(1) shouldEqual
      3
  }

  it can "find the greater node alternatively" in {
    inverted.greaterNode shouldEqual
      3
  }

  val straight: Edge =
      1--3

  "An edge with smaller node as first param at creation" must "have as first node always the smaller of the two" in {
    straight.nodes.head shouldEqual
      1
  }

  it can "find the lesser node alternatively" in {
    straight.lesserNode shouldEqual
      1
  }

  it must "have as second node always the greater of the two" in {
    straight.nodes(1) shouldEqual
      3
  }

  it can "find the greater node alternatively" in {
    straight.greaterNode shouldEqual
      3
  }

  "An Edge" can "be compared to an identical one" in {
    EdgeOrdering.compare(3--1, 3--1) shouldEqual
      0
  }

  it can "be compared to an identical one even if created differently" in {
    EdgeOrdering.compare(3--1, 1--3) shouldEqual
      0
  }

  it can "be compared to an higher one, with an higher greater node" in {
    EdgeOrdering.compare(1--4, 1--3) shouldEqual
      1
  }

  it can "be compared to an lower one, with a lower greater node" in {
    EdgeOrdering.compare(1--2, 1--3) shouldEqual
      -1
  }

  it can "be compared to an higher one, with an higher lesser node" in {
    EdgeOrdering.compare(4--6, 3--6) shouldEqual
      1
  }

  it can "be compared to an lower one, with a lower lesser node" in {
    EdgeOrdering.compare(4--6, 5--6) shouldEqual
      -1
  }

  it must "be different from a node" in {
    anEdge should not equal
      anEdge.lesserNode
  }

  val second: Edge =
    3--1
  val third: Edge =
    2--3

  val unsorted: List[Edge] =
    List(second, third, anEdge)

  "A list of Edge" can "be ordered" in {
    unsorted.sorted(EdgeOrdering) shouldEqual
      List(anEdge, second, third)
  }

  it can "be made safely unique" in {
    List(2--8, 2--8).distinct shouldBe
      List(2--8)
  }

  it can "be made safely unique even if creation differs" in {
    List(2--8, 8--2).distinct shouldBe
      List(2--8)
  }

  it can "be printed as string" in {
    List(anEdge, second).stringify shouldBe
      "1--2, 1--3"
  }

  it can "return their distinct nodes" in {
    List(anEdge, second).nodes shouldBe
      List(1, 2, 3)
  }

  it can "be equal by size of edges and by size of distinct nodes" in {
    EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(List(anEdge, second), List(anEdge, second)) shouldBe
      0
  }

  it can "differ by size of edges" in {
    EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(List(anEdge, second), List(anEdge)) shouldBe
      1
  }

  it can "be equal by size of edges and differ by size of distinct nodes" in {
    EdgesSizeOrdering.orElse(EdgesNodesSizeOrdering).compare(List(anEdge, second),List(anEdge, 3--4)) shouldBe
      -1
  }

  it can "return the degree of a given node" in {
    List(second, third, anEdge).degree(Node(1)) shouldBe
      Degree(2)
  }

  it can "return a map of all degrees" in {
    List(second, third, anEdge).allDegrees shouldBe
      Map(
        Node(1) -> Degree(2),
        Node(2) -> Degree(2),
        Node(3) -> Degree(2)
      )
  }

  it can "return every node adjacent to a given node" in {
    List(second, third, anEdge).adjacentTo(Node(1)) shouldBe
      List(3, 2)
  }

  it can "be filtered to just those containing only a given set of nodes" in {
    List(anEdge, second, third).withOnlyNodes(List(Node(1), Node(2))) shouldBe
      List(anEdge)
  }

  it can "be filtered to just those containing a given set of nodes" in {
    List(anEdge, second, third).withNodes(List(Node(1), Node(2))) shouldBe
      List(anEdge, second, third)
  }

  it can "be filtered to just those not containing a given set of nodes" in {
    List(anEdge, second, third).withoutNodes(List(Node(1), Node(2))) shouldBe
      List()
  }

  it can "be filtered to just those not containing only a given set of nodes" in {
    List(anEdge, second, third).withoutOnlyNodes(List(Node(1), Node(2))) shouldBe
      List(second, third)
  }

  val notCompactSet: List[Edge] =
    List(anEdge, third, 5--6)
    
  "A not compact set" can "be compacted after a given node" in {
    notCompactSet.compactAfter(Node(3)) shouldEqual
      List(anEdge, third, 4--5)
  }

  it can "be compacted starting from a given node" in {
    notCompactSet.compactStartingAt(Node(3)) shouldEqual
      List(3--4, 4--5, 6--7)
  }

  it can "be compacted starting from Node 1" in {
    notCompactSet.compact shouldEqual
      List(1--2, 2--3, 4--5)
  }

  "A compact set" can "be already compacted" in {
    List(anEdge, third, 4--5).compactAfter(Node(3)) shouldEqual
      List(anEdge, third, 4--5)
  }
  it can "be compacted starting from a given node" in {
    List(anEdge, third, 4--5).compactStartingAt(Node(3)) shouldEqual
      List(3--4, 4--5, 6--7)
  }

  "Another set" can "be compacted after a given node" in {
    List(1--3, 3--9).compactAfter(Node(3)) shouldEqual
      List(1--3, 3--4)
  }

  val sqrEdges: List[Edge] =
    p4444_4by4_reticulate.graphEdges

//  it can "be searched with breadth-first" in {
//    sqrEdges.bfs(Node(1)) shouldEqual
//      List(1, 2, 6, 7, 11, 12, 16, 17, 21, 22, 3, 8, 13, 18, 23, 4, 9, 14, 19, 24, 5, 10, 15, 20, 25)
//    sqrEdges.bfs(Node(1), Option(Node(25))) shouldEqual
//      List(1, 2, 3, 4, 5, 10, 15, 20, 25)
//  }

  it can "be searched for a distance" in {
    sqrEdges.distance(Node(1), Node(25)) shouldEqual
      8
  }

  it can "be searched for another distance" in {
    sqrEdges.distance(Node(1), Node(1)) shouldEqual
      0
  }

  it can "be searched for a distance to a non existing node" in {
    sqrEdges.distance(Node(1), Node(100)) shouldEqual
      -1
  }

  it can "be searched for a distance avoiding some given nodes" in {
    sqrEdges.distance(Node(1), Node(4), List(Node(3))) shouldEqual
      5
  }

  it can "be searched for the nodes connected to a give node" in {
    sqrEdges.nodesConnectedTo(Node(1)) shouldEqual
      List(1, 2, 6, 7, 11, 12, 16, 17, 21, 22, 3, 8, 13, 18, 23, 4, 9, 14, 19, 24, 5, 10, 15, 20, 25)
  }

  it can "be searched for circular nodes" in {
    List(1--2, 2--7, 7--6, 6--1).maybeCircularNodes(_.headOption) shouldEqual
      Option(Vector(1, 6, 7, 2))
  }
  
  it can "fail to find circular nodes" in {
    List(1--2, 6--1).maybeCircularNodes(_.headOption) shouldEqual
      None
  }

  val disconnect: List[Edge] =
    List(
      4--5, 5--6, 6--7, 7--8, 11--12, 1--8, 1--4, 12--18, 11--18, 18--19,
      11--19, 12--41, 18--41, 18--42, 19--42, 41--61, 18--61, 42--61
    )

  "A different set" can "be searched for disconnected nodes" in {
    disconnect.disconnectedNodes shouldBe
      List(
        List(11, 12, 18, 19, 41, 42, 61),
        List(4, 5, 6, 7, 8, 1)
      )
  }

  it can "be checked if all connected" in {
    disconnect.areConnected shouldBe
      false
  }

  it can "return the disconnected edges" in {
    disconnect.disconnected shouldBe
      List(
        List(11--12, 12--18, 11--18, 18--19, 11--19, 12--41, 18--41, 18--42, 19--42, 41--61, 18--61, 42--61),
        List(4--5, 5--6, 6--7, 7--8, 1--8, 1--4)
      )
  }

  "Other edges" can "be checked if all connected" in {
    sqrEdges.areConnected shouldBe
      true
  }

  val withDuplicatedEdges: List[Edge] =
    List(1--2, 2--3, 3--2, 2--7, 7--6, 6--1)

  "Set with duplicated edges" can "be checked for a node's degree" in {
    withDuplicatedEdges.degree(Node(2)) shouldBe
      Degree(4)
  }

  it can "be checked for all degrees" in {
    withDuplicatedEdges.allDegrees shouldBe
      Map(
        Node(1) -> Degree(2),
        Node(6) -> Degree(2),
        Node(2) -> Degree(4),
        Node(7) -> Degree(2),
        Node(3) -> Degree(2)
      )
  }

}
