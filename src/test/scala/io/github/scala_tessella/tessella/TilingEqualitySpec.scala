package io.github.scala_tessella.tessella

import Outliers.{edges12Nodes8, edges12Nodes8Similar, minimalDifferentFromItsPeri, p4444_4by4_reticulate}
import TilingGrowth.*
import Topology.{--, Edge, Node}

import io.github.scala_tessella.ring_seq
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class TilingEqualitySpec extends AnyFlatSpec with Helper with ring_seq.IteratingOps with should.Matchers {

  val onePgonTenEdges: Tiling =
    Tiling.fromPolygon(10)

  val twoPgonsFiveEdges: Tiling =
    minimalDifferentFromItsPeri.toMaybeTiling.toOption.get

  "Tilings" must "be first ordered by ascending count of polygons" in {
    List(twoPgonsFiveEdges, onePgonTenEdges).sorted shouldBe
      List(onePgonTenEdges, twoPgonsFiveEdges)
  }
  val twoPgonsSevenEdges: Tiling =
    Tiling.pattern_4444(1, 2).unsafe

  "Two tilings" can "have the same number of polygons" in {
    twoPgonsSevenEdges.countPolygons shouldEqual
      twoPgonsFiveEdges.countPolygons
  }
  
  they can "have a different count of edges" in {
    twoPgonsSevenEdges.graphEdges.size should not equal
      twoPgonsFiveEdges.graphEdges.size
  }
  
  they must "be then ordered by ascending count of edges, if count of polygons is equal" in {
    List(twoPgonsSevenEdges, twoPgonsFiveEdges).sorted shouldBe
      List(twoPgonsFiveEdges, twoPgonsSevenEdges)
  }

  val tiling1: Tiling =
    Tiling.maybe(
      2 -- 8, 2 -- 3, 3 -- 4, 4 -- 5, 5 -- 6, 6 -- 7, 7 -- 8, 1 -- 7, 1 -- 5, 1 -- 4, 1 -- 3, 1 -- 2, 2 -- 9, 8 -- 9, 9 -- 10, 10 -- 11, 8 -- 11, 11 -- 12,
      8 -- 12, 7 -- 12
    ).unsafe

  val tiling2: Tiling =
    Tiling.maybe(
      2--8, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 1--7, 1--5, 1--4, 1--3, 1--2, 2--9, 8--9, 9--10, 8--10, 10--11,
      11--12, 8--12, 7--12
    ).unsafe

  "Other two tilings" can "have the same number of polygons" in {
    tiling1.countPolygons shouldEqual
      tiling2.countPolygons
  }

  they can "have the same number of edges" in {
    tiling1.graphEdges.size shouldEqual
      tiling2.graphEdges.size
  }

  they must "be then ordered by ascending list of perimeter edge polygons, if all other is equal" in {
    List(tiling2, tiling1).sorted shouldBe
      List(tiling1, tiling2)
  }

  val twoSquares: Tiling =
    Tiling.pattern_4444(1, 2).unsafe

  "A Tiling" must "be equal to itself" in {
    twoSquares shouldEqual twoSquares
  }

  it can "have a different edge count from another" in {
    twoSquares.graphEdges.size should not equal
      hexagon.graphEdges.size
  }

  it must "be different from other with different edges size" in {
    twoSquares should not equal
      hexagon
  }

  val eptagon: Tiling =
    Tiling.fromPolygon(7)

  it can "have the same edge count of another" in {
    twoSquares.graphEdges.size shouldEqual
      eptagon.graphEdges.size
  }

  it can "have a different node count from another" in {
    twoSquares.graphNodes.size should not equal
      eptagon.graphNodes.size
  }

  it must "be different from other with different nodes size" in {
    twoSquares should not equal
      eptagon
  }

  "Two tilings" can "have the same edge count" in {
    edges12Nodes8.graphEdges.size shouldEqual
      edges12Nodes8Similar.graphEdges.size
  }
  
  they can "have the same node count" in {
    edges12Nodes8.graphNodes.size shouldEqual
      edges12Nodes8Similar.graphNodes.size
  }

  they can "have different angles at the perimeter" in {
    edges12Nodes8.orderedPerimeterAngles.rotations.contains(edges12Nodes8Similar.orderedPerimeterAngles) shouldBe
      false
  }

  they must "be different one from the other because of different angles at perimeter" in {
    edges12Nodes8 should not equal
      edges12Nodes8Similar
  }

  "Two square nets" must "be equal" in {
    Tiling.pattern_4444(4).unsafe shouldEqual
      p4444_4by4_reticulate
  }

  "Two tilings" must "be equal in rounded path angles" in {
    val first: Option[Tiling] =
      Tiling.maybe(
        2--8, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 1--7, 1--5, 1--4, 1--3, 1--2, 5--9, 9--10, 10--11, 11--12, 12--13,
        13--14, 14--15, 15--16, 16--17, 17--18, 4--18, 18--19, 3--19
      ).toOption
    val second: Option[Tiling] =
      Tiling.maybe(
        2--15, 2--3, 3--4, 4--5, 5--6, 6--7, 7--8, 8--9, 9--10, 10--11, 11--12, 12--13, 13--14, 14--15, 1--6, 1--4,
        1--3, 1--2, 4--16, 3--16, 16--17, 17--18, 3--18, 18--19, 2--19
      ).toOption
    first shouldEqual
      second
  }

  val baseEdges: List[Edge] =
    List(
      2--7, 2--3, 3--4, 4--5, 5--6, 6--7, 1--7, 1--6, 1--5, 1--4,
      1--3, 1--2, 2--8, 8--9, 9--10, 10--11, 11--12, 7--12
    )
  val tiling1almostSame: Tiling =
    Tiling.maybe(List(3--8, 2--12) ++ baseEdges).toOption.get
  val tiling2almostSame: Tiling =
    Tiling.maybe(List(7--8, 6--12) ++ baseEdges).toOption.get

  "Two tilings with different edges" can "be evaluated with ==" in {
    tiling1almostSame == tiling2almostSame shouldBe
      true
  }

  they can "be evaluated as equals" in {
    tiling1almostSame.equals(tiling2almostSame) shouldBe
      true
  }

  they can "be compared" in {
    tiling1almostSame.compare(tiling2almostSame) shouldBe
      0
  }

  val listed: List[Tiling] =
    List(tiling1almostSame, tiling2almostSame)
  
  they can "have the same hash code" in {
    listed.map(_.hashCode()).distinct.size shouldBe
      1
  }

  they can "have a hash code for the first one" in {
    tiling1almostSame.hashCode() shouldBe
      -425435535
  }

  they can "have a hash code for the second one" in {
    tiling2almostSame.hashCode() shouldBe
      -425435535
  }

//  they can "be seen as same with dedicated method" in {
//    Tiling.distinctSafe(listed).size shouldBe
//      1
//  }

  val t: Tiling =
    p4444_4by4_reticulate.maybeRemoveNode(Node(21)).flatMap(_.maybeRemoveNode(Node(5))).unsafe

  "A tiling" can "have two separate inner tilings" in {
    t.nestedTilings(isStrict = false) shouldBe
      List(
        List(
          List(
            List(12--17, 17--18, 13--18, 12--13)
          ),
          List(
            List(7--12, 6--7, 6--11, 11--12)
          )
        )
      )
  }

  "Another tiling" can "have only one inner tiling" in {
    p4444_4by4_reticulate.nestedTilings(isStrict = false) shouldBe
      List(
        List(
          List(
            List(12--13, 7--12, 7--8, 8--13),
            List(13--18, 17--18, 12--17, 12--13),
            List(8--13, 8--9, 9--14, 13--14),
            List(13--14, 14--19, 18--19, 13--18)
          )
        )
      )
  }

  val aTriangleNet: Tiling =
    Tiling.pattern_333333(6, 3).unsafe

  "A triangle net" can "return the concentric polygons" in {
    aTriangleNet.nestedTilings(isStrict = false) shouldBe
      List(
        List(
          List(
            List(6--11, 7--11, 6--7),
            List(6--11, 6--10, 10--11)
          )
        )
      )
  }

  it can "return the strictly concentric polygons" in {
    aTriangleNet.nestedTilings(isStrict = true) shouldBe
      List(
        List(
          List(
            List(9--10, 9--14, 10--14)
          ),
          List(
            List(7--11, 11--12, 7--12),
            List(6--11, 7--11, 6--7),
            List(2--7, 2--6, 6--7),
            List(10--15, 11--15, 10--11),
            List(6--11, 6--10, 10--11),
            List(5--10, 6--10, 5--6)
          ),
          List(
            List(3--8, 3--7, 7--8)
          )
        ),
        List(
          List(
            List(6--11, 7--11, 6--7),
            List(6--11, 6--10, 10--11)
          )
        )
      )
  }

  it can "return the strictly concentric perimeters" in {
    aTriangleNet.nestedPerimeters(isStrict = true) shouldBe
      List(
        List(
          Vector(1, 5, 9, 13, 14, 15, 16, 12, 8, 4, 3, 2)
        ),
        List(
          Vector(10, 9, 14),
          Vector(5, 10, 15, 11, 12, 7, 2, 6),
          Vector(7, 3, 8)
        ),
        List(
          Vector(6, 7, 11, 10)
        )
      )
  }

}
