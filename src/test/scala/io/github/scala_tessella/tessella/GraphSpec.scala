package io.github.scala_tessella.tessella

import Outliers.*
import Topology.*
import conversion.DOT.toDOT
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GraphSpec extends AnyFlatSpec with should.Matchers {

  val reticulate: Graph =
    Graph(p4444_4by4_reticulate.graphEdges)

  "A path of a graph" can "be created" in {
    reticulate.Path(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25).map(Node(_))) shouldEqual
      Vector(1, 2, 3, 4, 5, 10, 15, 20, 25)
  }

  it can "be created of length 1" in {
    reticulate.Path(Vector(1).map(Node(_))) shouldEqual
      Vector(1)
  }

  it can "be created empty" in {
    reticulate.Path(Vector()) shouldEqual
      Vector.empty
  }

  it must "fail if of size 1 and with the node not part of the graph" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.Path(Vector(100).map(Node(_)))
    }
    caught.getMessage shouldBe "Invalid path: node 100 doesn't belong to graph"
  }

  it must "fail if any node is not part of the graph" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.Path(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 100).map(Node(_)))
    }
    caught.getMessage shouldBe "Invalid path: node 100 doesn't belong to graph"
  }

  val notConnected: Vector[Node] =
    Vector(1, 3, 4, 5, 10, 15, 20, 25).map(Node(_))

  it must "fail if not connected" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.Path(notConnected)
    }
    caught.getMessage shouldBe "Invalid path: nodes 1 and 3 are not connected"
  }

  it must "not fail if created unsafe and not connected" in {
    reticulate.Path.unsafe(notConnected) shouldEqual
      Vector(1, 3, 4, 5, 10, 15, 20, 25)
  }

  it must "not fail if created unsafe and with node not belonging to graph" in {
    reticulate.Path.unsafe(Vector(Node(100))) shouldEqual
      Vector(100)
  }

  val aPath: reticulate.Path =
    reticulate.Path(Vector(1, 2, 3).map(Node(_)))

  "A path" can "be converted to edges" in {
    aPath.toPathEdges.toList shouldEqual
      List(1 -- 2, 2 -- 3)
  }

  val aRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(1, 2, 7, 6).map(Node(_)))

  "A simple path" can "be inferred from edges" in {
    reticulate.Path.maybeSimpleFrom(List(1 -- 2, 2 -- 3, 3 -- 4)) shouldBe
      Option(reticulate.Path(Vector(1, 2, 3, 4).map(Node(_))))
  }
  it can "fail to be inferred from edges, since node 7 is at a fork" in {
    reticulate.Path.maybeSimpleFrom(List(1 -- 2, 2 -- 3, 3 -- 8, 7 -- 8, 2 -- 7, 6 -- 7)) shouldBe
      None
  }

  it can "fail to be inferred from edges, because it is a ring" in {
    reticulate.Path.maybeSimpleFrom(List(1--2, 2--7, 6--7, 1--6)) shouldBe
      None
  }

  "A circular path of a graph" can "be created" in {
    aRingPath shouldEqual
      Vector(1, 2, 7, 6)
  }

  it can "be created of length 1" in {
    reticulate.RingPath(Vector(1).map(Node(_))) shouldEqual
      Vector(1)
  }

  it can "be created empty" in {
    reticulate.RingPath(Vector()) shouldEqual
      Vector.empty
  }

  it must "fail if of size 1 and not present" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.RingPath(Vector(100).map(Node(_)))
    }
    caught.getMessage shouldBe "Invalid path: node 100 doesn't belong to graph"
  }

  it must "fail if not present" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.RingPath(Vector(1, 2, 7, 100).map(Node(_)))
    }
    caught.getMessage shouldBe "Invalid path: node 100 doesn't belong to graph"
  }

  it must "fail if not connected" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.RingPath(Vector(1, 3, 8, 7).map(Node(_)))
    }
    caught.getMessage shouldBe "Invalid path: nodes 1 and 3 are not connected"
  }

  val notCircular: Vector[Node] =
    Vector(1, 2, 7, 8).map(Node(_))

  it must "fail if not circular" in {
    val caught = intercept[IllegalArgumentException] {
      reticulate.RingPath(notCircular)
    }
    caught.getMessage shouldBe "Invalid path: nodes 8 and 1 are not connected"
  }

  it must "not fail if created unsafe" in {
    reticulate.RingPath.unsafe(notCircular) shouldEqual
      Vector(1, 2, 7, 8)
  }

  val circularEdges: List[Edge] =
    List(7--6, 2--7, 1--2, 6--1)

  "An optional circular path" can "be created from a subset of edges" in {
    reticulate.RingPath.maybeSimpleFromEdges(circularEdges) shouldEqual
      Option(Vector(6, 1, 2, 7))
  }

  it must "fail to be created if not circular" in {
    reticulate.RingPath.maybeSimpleFromEdges(circularEdges.tail) shouldEqual
      None
  }

  it must "fail to be created if subset edge not present" in {
    reticulate.RingPath.maybeSimpleFromEdges(List(1--2, 2--7, 7--100, 100--1)) shouldEqual
      None
  }

  "A circular path" can "be created from a subset of edges" in {
    reticulate.RingPath.simpleFromEdges(circularEdges) shouldEqual
      Vector(6, 1, 2, 7)
  }

  val aSimpleRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(1, 2, 7, 6).map(Node(_)))

  "A circular path" can "be converted to edges" in {
    aSimpleRingPath.toRingEdges.toList shouldEqual
      List(1--2, 2--7, 7--6, 6--1)
  }

  "An edge" can "be checked for having the same orientation of the circular path" in {
    aSimpleRingPath.isOrientedAt(1--2) shouldBe
      Some(true)
  }

  "Another edge" can "NOT be checked since does not belong to the circular path" in {
    aSimpleRingPath.isOrientedAt(1--3) shouldBe
      None
  }

  "A third edge" can "be checked for having the opposite orientation of the circular path" in {
    aSimpleRingPath.isOrientedAt(6--7) shouldBe
      Some(false)
  }

  "A simple circular path" can "be checked as simple" in {
    aSimpleRingPath.isSimple shouldBe
      true
  }

  val aTwoNodesRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(1, 2).map(Node(_)))

  "A two nodes circular path" must "be valid" in {
    aTwoNodesRingPath.toRingNodes shouldBe
      Vector(1, 2)
  }

  val aComplexRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(1, 2, 3, 2, 7, 6).map(Node(_)))

  "A complex circular path" can "be checked as NOT simple" in {
    aComplexRingPath.isSimple shouldBe
      false
  }

  val anotherSimpleRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(2, 7, 8, 3).map(Node(_)))

  val aThirdSimpleRingPath: reticulate.RingPath =
    reticulate.RingPath(Vector(8, 3, 4, 9).map(Node(_)))

  "Unoriented circular paths" can "be oriented" in {
    List(aSimpleRingPath, anotherSimpleRingPath, aThirdSimpleRingPath).oriented shouldBe
      List(
        Vector(1, 2, 7, 6),
        Vector(3, 8, 7, 2),
        Vector(8, 3, 4, 9)
      ).map(nodes => reticulate.RingPath(nodes.map(Node(_))))
  }

  they can "NOT be oriented if no shared edge" in {
    List(aSimpleRingPath, aThirdSimpleRingPath).oriented shouldBe
      List(
        Vector(1, 2, 7, 6),
        Vector(8, 3, 4, 9)
      ).map(nodes => reticulate.RingPath(nodes.map(Node(_))))
  }

  val graph: Graph =
    Graph(triangle.graphEdges)

  "A Graph" can "be printed" in {
    graph.toString shouldBe
      "Graph(1--2, 1--3, 2--3)"
  }

  it can "have the nodes differing from the compacted version listed" in {
    graph.diffFromCompacted shouldEqual
      Nil
  }

  it can "be checked for being compacted" in {
    graph.isCompacted shouldBe
      true
  }

  it can "be checked for being connected" in {
    graph.isConnected shouldBe
      true
  }

  it can "be printed in DOT notation" in {
    graph.toDOT() shouldEqual
      """graph{
        |1 -- 2
        |2 -- 3
        |1 -- 3
        |}""".stripMargin
  }

  it can "be compacted" in {
    Graph(graph.graphEdges.withoutNodes(List(Node(2)))).compact.toString shouldBe
      "Graph(1--2)"
  }

  "An example for the oriented method" can "be tested" in {
    val g: Graph =
      Graph(1--2, 1--3, 2--3, 1--4, 2--4)
    val r123: g.RingPath =
      g.RingPath(Vector(Node(1), Node(2), Node(3)))
    val r124: g.RingPath =
      g.RingPath(Vector(Node(1), Node(2), Node(4)))
    List(r123, r124).oriented shouldBe
      List(Vector(3, 2, 1), Vector(1, 2, 4))
  }

  "A complete graph" can "be created" in {
    Graph.complete(5).graphEdges shouldBe
      List(1--2, 1--3, 1--4, 1--5, 2--3, 2--4, 2--5, 3--4, 3--5, 4--5)
  }

//  "Double triangles" can "be found and extracted from the perimeter of a graph" in {
//    val t: Tiling =
//      FullVertex.s("(3.3.4.3.4)").toTiling
//    t.edges shouldBe
//      List(2--8, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 1--7, 1--6, 1--4, 1--3, 1--2)
//    val remainingEdges: List[Edge] =
//      t.edges.filterNot(_.containsNode(Node(3)))
//    remainingEdges shouldBe
//      List(2--8, 4--5, 5--6, 6--7, 7--8, 1--7, 1--6, 1--4, 1--2)
//    t.extractPerimeterCoupledTriangles shouldBe
//      Some(remainingEdges, List(Vector(1, 2, 3), Vector(1, 4, 3)))
//        .map((es, rings) => (es, rings.map(_.map(Node(_)))))
//    val h: Graph =
//      Graph(remainingEdges)
//    h.extractPerimeterCoupledTriangles shouldBe
//      None
//  }
//
//  "Double triangles" must "NOT be found if they disconnect the graph" in {
//    val t: Tiling =
//      Tiling.fromVertex(Vertex(Vector(4, 3, 3, 4).map(Polygon(_))))
//    t.extractPerimeterCoupledTriangles shouldBe
//      None
//  }
//
//  "A square with two shared edges" can "be found" in {
//    val t: Tiling =
//      FullVertex.s("(3.3.4.3.4)").toTiling
//    t.extractPerimeterPolygons shouldBe
//      Some((List(2--8, 2--3, 3--4, 6--7, 7--8, 1--7, 1--6, 1--4, 1--3, 1--2), List(Vector(4, 5, 6, 1))))
//  }
//
//  it can "be found in another tiling" in {
//    val t: Tiling =
//      FullVertex.s("(3.3.3.4.4)").toTiling
//    t.extractPerimeterPolygons shouldBe
//      Some((List(2--8, 2--3, 3--4, 4--5, 7--8, 1--7, 1--5, 1--4, 1--3, 1--2), List(Vector(7, 6, 5, 1))))
//  }
//
//  "A triangle" can "be found in another tiling" in {
//    val t: Tiling =
//      Tiling.fromVertex(Vertex(Vector(3, 4, 3).map(Polygon(_))))
//    t.extractPerimeterPolygons shouldBe
//      Some((List(2--3, 3--4, 4--5, 1--5, 1--3, 1--2), List(Vector(1, 6, 5))))
//  }
//
//  "A hexagon with two shared edges" can "be found" in {
//    val t: Tiling =
//      FullVertex.s("(6.6.6)").toTiling
//    t.extractPerimeterPolygons shouldBe
//      Some(List(2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 9--10, 1--10, 1--6, 1--2), List(Vector(2, 13, 12, 11, 10, 1)))
//  }
//
//  it can "be found in another tiling" in {
//    val t: Tiling =
//      Tiling.hexagonNet(2, 2).unsafe
//    t.extractPerimeterPolygons shouldBe
//      Some(
//        List(6--7, 12--13, 1--2, 7--8, 13--14, 2--3, 8--9, 14--15, 9--10, 15--16, 10--11, 1--6, 7--12, 3--8, 9--14, 11--16),
//        List(Vector(3, 4, 5, 10, 9, 8))
//      )
//  }
//
//  "A perimeter inserted triangle" can "be found and extracted" in {
//    val t: Tiling =
//      FullVertex.s("(3.6.3.6)").toTiling
//    t.edges shouldBe
//      List(2--11, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 1--8, 1--7, 1--3, 1--2)
//    val remainingEdges: List[Edge] =
//      t.edges.filterNot(_ == 2--3)
//    remainingEdges shouldBe
//      List(2--11, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 1--8, 1--7, 1--3, 1--2)
////    t.extractPerimeterStuckTriangle(false) shouldBe
////      Some(remainingEdges, Vector(2, 3, 1))
////    t.extractPerimeterStuckTriangle(true) shouldBe
////      Some(remainingEdges, Vector(2, 3, 1))
//    t.extractPerimeterStuckTriangle(false) shouldBe
//      Some(remainingEdges, List(Vector(2, 1, 3)))
//  }
//
//  it can "be found in another graph" in {
//    val t: Tiling =
//      FullVertex.s("(3.3.3.3.3.3)").toTiling
////    t.extractPerimeterStuckTriangle(false) shouldBe
////      None
////    t.extractPerimeterStuckTriangle(true) shouldBe
////      None
//    t.extractPerimeterStuckTriangle() shouldBe
//      None
//    t.extractPerimeterStuckTriangle2 shouldBe
//      Some(List(2--7, 2--3, 3--4, 4--5, 6--7, 1--7, 1--6, 1--5, 1--4, 1--3, 1--2), List(Vector(1, 5, 6)))
//  }
//
//  it must "NOT be found if it disconnects the graph" in {
//    val t: Tiling =
//      Tiling.fromVertex(Vertex(Vector(4, 3, 4).map(Polygon(_))))
////    t.extractPerimeterStuckTriangle(true) shouldBe
////      None
//    t.extractPerimeterStuckTriangle() shouldBe
//      None
//    t.extractPerimeterStuckTriangle2 shouldBe
//      None
//  }

  "A perimeter RingPath" can "be found with the current method" in {
    reticulate.tilingOrientedPerimeter shouldBe
      Option(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 24, 23, 22, 21, 16, 11, 6))
  }

  "The edges of a perimeter RingPath" can "be detected" in {
    reticulate.tilingOrientedPolygons.map(_.toPerimeterEdges).get shouldBe
      List(
        20--25, 3--4, 10--15, 1--2, 5--10, 1--6, 22--23, 23--24,
        24--25, 15--20, 2--3, 11--16, 4--5, 16--21, 6--11, 21--22
      )
  }

  "The edges of a perimeter RingPath" can "be transformed into a rotated ring with a simpler method, not used now" in {
    reticulate.RingPath.maybeSimpleFromEdges(reticulate.tilingOrientedPolygons.map(_.toPerimeterEdges).get) shouldBe
      Option(Vector(20, 15, 10, 5, 4, 3, 2, 1, 6, 11, 16, 21, 22, 23, 24, 25))
  }

  "The edges of a perimeter RingPath" can "be transformed into a ring with the current method" in {
    reticulate.RingPath.maybeSimpleFromEdges2(reticulate.tilingOrientedPolygons.map(_.toPerimeterEdges).get) shouldBe
      Option(Vector(1, 2, 3, 4, 5, 10, 15, 20, 25, 24, 23, 22, 21, 16, 11, 6))
  }

}
