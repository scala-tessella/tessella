package io.github.scala_tessella.tessella

import Geometry.Radian.{TAU_2, TAU_3, TAU_4}
import RegularPolygon.{Polygon, PolygonOrdering, PolygonsSeqOrdering, Vertex}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.math.Ordering.Implicits.seqOrdering

class RegularPolygonSpec extends AnyFlatSpec with Accuracy with should.Matchers {

  "A Polygon" must "be valid" in {
    val caught = intercept[IllegalArgumentException] {
      Polygon(2)
    }
    caught.getMessage shouldBe "Invalid number of sides: 2"
  }

  it can "be compared to another one" in {
    PolygonOrdering.compare(Polygon(4), Polygon(8)) shouldEqual
      -1
  }

  it must "have an interior angle" in {
    Polygon(4).alpha shouldEqual
      TAU_4
  }

  "A triangle" must "have an area" in {
    (Polygon(3).area ~= 0.43301270189221946) shouldBe
      true
  }

  "A square" must "have an area" in {
    (Polygon(4).area ~= 1.0) shouldBe
      true
  }

  "A hexagon" must "have an area" in {
    (Polygon(6).area ~= 6 * 0.43301270189221946) shouldBe
      true
  }

  "An octagon" must "have an area" in {
    (Polygon(8).area ~= 4.82842712474619) shouldBe
      true
  }

  "A sequence of Polygon" can "be compared to another one" in {
    PolygonsSeqOrdering.compare(List(Polygon(4), Polygon(8)), List(Polygon(3))) shouldEqual
      1
  }

  it must "have a sum of interior angles" in {
    Vector(Polygon(4), Polygon(4)).alphaSum shouldEqual
      TAU_2
  }

  "A valid Vertex" can "be created as an opaque type" in {
    Vertex(Polygon(3), Polygon(7)).isPartial shouldBe
      true
  }

  it can "be created as a sequence of polygons" in {
    Vertex(Polygon(3), Polygon(7), Polygon(42)).isFull shouldBe
      true
  }

  it can "be created as a sequence of polygon sides" in {
    Vertex(3, 7, 42).isFull shouldBe
      true
  }

  it can "be created empty" in {
    Vertex.empty.isPartial shouldBe
      true
  }

  "An invalid vertex" must "fail" in {
    val caught = intercept[IllegalArgumentException] {
      Vertex(7, 7, 42)
    }
    caught.getMessage shouldBe "More than full vertex: Vector(7, 7, 42)"
  }

  "Full vertices" can "be found assembling regular triangles" in {
    Vertex.fullVerticesBy(List(Polygon(3))) shouldBe
      List(Vertex(3, 3, 3, 3, 3, 3))
  }

  they can "be found assembling regular pentagons" in {
    Vertex.fullVerticesBy(List(Polygon(5))) shouldBe
      Nil
  }

  they can "be found assembling regular triangles and squares" in {
    Vertex.fullVerticesBy(List(3, 4).map(Polygon(_))) shouldBe
      List(
        Vertex(3, 3, 3, 3, 3, 3),
        Vertex(3, 3, 3, 4, 4),
        Vertex(4, 4, 4, 4)
      )
  }

  "Full vertices" can "be found including permutations" in {
    Vertex.fullVerticesWithPermutationsBy(List(3, 4).map(Polygon(_))) shouldBe
      List(
        Vertex(3, 3, 3, 3, 3, 3),
        Vertex(3, 3, 3, 4, 4),
        Vertex(3, 3, 4, 3, 4),
        Vertex(4, 4, 4, 4)
      )
  }

  "Only 17 distinct full vertices" can "be found assembling any size of regular polygons" in {
    val total: List[Vector[Polygon]] =
      Vertex.fullVerticesBy((3 to 50).toList.map(Polygon(_)))
    total.size shouldBe
      17
  }

  "Only 21 distinct full vertices" can "be found when counting permutations" in {
    Vertex.fullMinors.size shouldBe
      21
  }

  they can "be listed" in {
    Vertex.fullMinors.map(FullVertex.maybe(_).toOption.get.toString) shouldEqual
      List(
        "(3₆)", "(3₃.4₂)", "(3₂.4.3.4)", "(4₄)", "(3₄.6)", "(3.4₂.6)", "(3.4.6.4)",
        "(3₂.6₂)", "(3.6.3.6)", "(6₃)", "(4.8₂)", "(5₂.10)", "(3₂.4.12)", "(3.4.3.12)",
        "(4.6.12)", "(3.12₂)", "(3.10.15)", "(3.9.18)", "(4.5.20)", "(3.8.24)", "(3.7.42)"
      )
  }

  "Only 3 full vertices" must "be monohedral" in {
    Vertex.monoHedrals.map(vertex => FullVertex.maybe(vertex.toPolygons).toOption.get.toString) shouldEqual
      List("(3₆)", "(4₄)", "(6₃)")
  }

  "Only 12 polygons" can "be tessellable" in {
    Vertex.tessellablePolygons shouldEqual
      List(3, 4, 5, 6, 7, 8, 9, 10, 12, 15, 18, 20, 24, 42)
  }

  they can "have their area mapped" in {
    (Vertex.tessellableAreas(Polygon(4)) ~= 1) shouldBe
      true
  }

  "Non tessellable" must "not be part of the map" in {
    Vertex.tessellableAreas.contains(Polygon(13)) shouldBe
      false
  }

  "All the full vertices rotations and reflections" must "be 93" in {
    Vertex.fullToMinor.size shouldBe
      93
  }

  they can "be mapped" in {
    Vertex.fullToMinor shouldBe
      Map(
        Vector(3, 4, 12, 3) -> Vector(3, 3, 4, 12),
        Vector(3, 42, 7) -> Vector(3, 7, 42),
        Vector(6, 4, 4, 3) -> Vector(3, 4, 4, 6),
        Vector(3, 9, 18) -> Vector(3, 9, 18),
        Vector(10, 15, 3) -> Vector(3, 10, 15),
        Vector(5, 10, 5) -> Vector(5, 5, 10),
        Vector(6, 3, 4, 4) -> Vector(3, 4, 4, 6),
        Vector(8, 24, 3) -> Vector(3, 8, 24),
        Vector(8, 3, 24) -> Vector(3, 8, 24),
        Vector(42, 7, 3) -> Vector(3, 7, 42),
        Vector(3, 3, 12, 4) -> Vector(3, 3, 4, 12),
        Vector(15, 10, 3) -> Vector(3, 10, 15),
        Vector(3, 6, 4, 4) -> Vector(3, 4, 4, 6),
        Vector(12, 3, 12) -> Vector(3, 12, 12),
        Vector(6, 12, 4) -> Vector(4, 6, 12),
        Vector(3, 3, 3, 4, 4) -> Vector(3, 3, 3, 4, 4),
        Vector(24, 3, 8) -> Vector(3, 8, 24),
        Vector(3, 3, 6, 6) -> Vector(3, 3, 6, 6),
        Vector(3, 3, 3, 3, 6) -> Vector(3, 3, 3, 3, 6),
        Vector(3, 10, 15) -> Vector(3, 10, 15),
        Vector(3, 12, 3, 4) -> Vector(3, 4, 3, 12),
        Vector(12, 3, 4, 3) -> Vector(3, 4, 3, 12),
        Vector(6, 4, 3, 4) -> Vector(3, 4, 6, 4),
        Vector(5, 20, 4) -> Vector(4, 5, 20),
        Vector(4, 6, 12) -> Vector(4, 6, 12),
        Vector(8, 4, 8) -> Vector(4, 8, 8),
        Vector(4, 6, 3, 4) -> Vector(3, 4, 4, 6),
        Vector(4, 3, 12, 3) -> Vector(3, 4, 3, 12),
        Vector(4, 20, 5) -> Vector(4, 5, 20),
        Vector(9, 3, 18) -> Vector(3, 9, 18),
        Vector(4, 3, 6, 4) -> Vector(3, 4, 4, 6),
        Vector(3, 8, 24) -> Vector(3, 8, 24),
        Vector(12, 4, 3, 3) -> Vector(3, 3, 4, 12),
        Vector(12, 3, 3, 4) -> Vector(3, 3, 4, 12),
        Vector(20, 5, 4) -> Vector(4, 5, 20),
        Vector(6, 3, 3, 6) -> Vector(3, 3, 6, 6),
        Vector(4, 4, 3, 3, 3) -> Vector(3, 3, 3, 4, 4),
        Vector(4, 12, 6) -> Vector(4, 6, 12),
        Vector(6, 6, 6) -> Vector(6, 6, 6),
        Vector(3, 18, 9) -> Vector(3, 9, 18),
        Vector(5, 5, 10) -> Vector(5, 5, 10),
        Vector(7, 42, 3) -> Vector(3, 7, 42),
        Vector(18, 3, 9) -> Vector(3, 9, 18),
        Vector(5, 4, 20) -> Vector(4, 5, 20),
        Vector(18, 9, 3) -> Vector(3, 9, 18),
        Vector(3, 4, 6, 4) -> Vector(3, 4, 6, 4),
        Vector(3, 3, 3, 3, 3, 3) -> Vector(3, 3, 3, 3, 3, 3),
        Vector(12, 12, 3) -> Vector(3, 12, 12),
        Vector(3, 3, 6, 3, 3) -> Vector(3, 3, 3, 3, 6),
        Vector(42, 3, 7) -> Vector(3, 7, 42),
        Vector(15, 3, 10) -> Vector(3, 10, 15),
        Vector(4, 3, 3, 12) -> Vector(3, 3, 4, 12),
        Vector(3, 12, 12) -> Vector(3, 12, 12),
        Vector(3, 3, 4, 12) -> Vector(3, 3, 4, 12),
        Vector(6, 4, 12) -> Vector(4, 6, 12),
        Vector(6, 6, 3, 3) -> Vector(3, 3, 6, 6),
        Vector(4, 3, 4, 6) -> Vector(3, 4, 6, 4),
        Vector(3, 6, 3, 6) -> Vector(3, 6, 3, 6),
        Vector(8, 8, 4) -> Vector(4, 8, 8),
        Vector(3, 4, 3, 4, 3) -> Vector(3, 3, 4, 3, 4),
        Vector(3, 6, 6, 3) -> Vector(3, 3, 6, 6),
        Vector(3, 6, 3, 3, 3) -> Vector(3, 3, 3, 3, 6),
        Vector(10, 3, 15) -> Vector(3, 10, 15),
        Vector(12, 6, 4) -> Vector(4, 6, 12),
        Vector(7, 3, 42) -> Vector(3, 7, 42),
        Vector(4, 3, 4, 3, 3) -> Vector(3, 3, 4, 3, 4),
        Vector(4, 4, 4, 4) -> Vector(4, 4, 4, 4),
        Vector(4, 12, 3, 3) -> Vector(3, 3, 4, 12),
        Vector(4, 3, 3, 3, 4) -> Vector(3, 3, 3, 4, 4),
        Vector(3, 3, 4, 4, 3) -> Vector(3, 3, 3, 4, 4),
        Vector(3, 4, 3, 3, 4) -> Vector(3, 3, 4, 3, 4),
        Vector(3, 24, 8) -> Vector(3, 8, 24),
        Vector(6, 3, 6, 3) -> Vector(3, 6, 3, 6),
        Vector(3, 4, 4, 3, 3) -> Vector(3, 3, 3, 4, 4),
        Vector(20, 4, 5) -> Vector(4, 5, 20),
        Vector(4, 6, 4, 3) -> Vector(3, 4, 6, 4),
        Vector(9, 18, 3) -> Vector(3, 9, 18),
        Vector(12, 4, 6) -> Vector(4, 6, 12),
        Vector(3, 7, 42) -> Vector(3, 7, 42),
        Vector(4, 4, 3, 6) -> Vector(3, 4, 4, 6),
        Vector(3, 12, 4, 3) -> Vector(3, 3, 4, 12),
        Vector(4, 3, 3, 4, 3) -> Vector(3, 3, 4, 3, 4),
        Vector(6, 3, 3, 3, 3) -> Vector(3, 3, 3, 3, 6),
        Vector(3, 4, 4, 6) -> Vector(3, 4, 4, 6),
        Vector(3, 4, 3, 12) -> Vector(3, 4, 3, 12),
        Vector(10, 5, 5) -> Vector(5, 5, 10),
        Vector(3, 15, 10) -> Vector(3, 10, 15),
        Vector(4, 5, 20) -> Vector(4, 5, 20),
        Vector(4, 8, 8) -> Vector(4, 8, 8),
        Vector(4, 4, 6, 3) -> Vector(3, 4, 4, 6),
        Vector(3, 3, 3, 6, 3) -> Vector(3, 3, 3, 3, 6),
        Vector(24, 8, 3) -> Vector(3, 8, 24),
        Vector(3, 3, 4, 3, 4) -> Vector(3, 3, 4, 3, 4)
      )
  }

  "All the partial vertices" must "be 114" in {
    Vertex.fillables.size shouldBe 114
  }

  they can "be mapped" in {
    Vertex.fillables shouldBe
      List(
        Vector(3),
        Vector(3, 3),
        Vector(3, 3, 3),
        Vector(3, 3, 3, 3),
        Vector(3, 3, 3, 3, 3),
        Vector(4),
        Vector(3, 4),
        Vector(4, 3),
        Vector(4, 4),
        Vector(3, 3, 4),
        Vector(4, 3, 3),
        Vector(3, 4, 4),
        Vector(4, 4, 3),
        Vector(3, 3, 3, 4),
        Vector(4, 3, 3, 3),
        Vector(3, 3, 4, 4),
        Vector(4, 4, 3, 3),
        Vector(3, 4, 4, 3),
        Vector(3, 4, 3),
        Vector(4, 3, 4),
        Vector(3, 3, 4, 3),
        Vector(3, 4, 3, 3),
        Vector(3, 4, 3, 4),
        Vector(4, 3, 4, 3),
        Vector(4, 3, 3, 4),
        Vector(4, 4, 4),
        Vector(5),
        Vector(5, 5),
        Vector(4, 5),
        Vector(5, 4),
        Vector(6),
        Vector(3, 6),
        Vector(6, 3),
        Vector(3, 3, 6),
        Vector(6, 3, 3),
        Vector(3, 6, 3),
        Vector(3, 3, 3, 6),
        Vector(6, 3, 3, 3),
        Vector(3, 3, 6, 3),
        Vector(3, 6, 3, 3),
        Vector(4, 6),
        Vector(6, 4),
        Vector(4, 4, 6),
        Vector(6, 4, 4),
        Vector(4, 6, 3),
        Vector(3, 6, 4),
        Vector(6, 3, 4),
        Vector(4, 3, 6),
        Vector(3, 4, 6),
        Vector(6, 4, 3),
        Vector(4, 6, 4),
        Vector(6, 6),
        Vector(3, 6, 6),
        Vector(6, 6, 3),
        Vector(6, 3, 6),
        Vector(7),
        Vector(3, 7),
        Vector(7, 3),
        Vector(8),
        Vector(4, 8),
        Vector(8, 4),
        Vector(8, 8),
        Vector(3, 8),
        Vector(8, 3),
        Vector(9),
        Vector(3, 9),
        Vector(9, 3),
        Vector(10),
        Vector(5, 10),
        Vector(10, 5),
        Vector(3, 10),
        Vector(10, 3),
        Vector(12),
        Vector(4, 12),
        Vector(12, 4),
        Vector(12, 3),
        Vector(3, 12),
        Vector(3, 4, 12),
        Vector(12, 4, 3),
        Vector(4, 12, 3),
        Vector(3, 12, 4),
        Vector(12, 3, 3),
        Vector(3, 3, 12),
        Vector(4, 3, 12),
        Vector(12, 3, 4),
        Vector(3, 12, 3),
        Vector(6, 12),
        Vector(12, 6),
        Vector(12, 12),
        Vector(15),
        Vector(10, 15),
        Vector(15, 10),
        Vector(15, 3),
        Vector(3, 15),
        Vector(18),
        Vector(9, 18),
        Vector(18, 9),
        Vector(18, 3),
        Vector(3, 18),
        Vector(20),
        Vector(5, 20),
        Vector(20, 5),
        Vector(20, 4),
        Vector(4, 20),
        Vector(24),
        Vector(8, 24),
        Vector(24, 8),
        Vector(24, 3),
        Vector(3, 24),
        Vector(42),
        Vector(7, 42),
        Vector(42, 7),
        Vector(42, 3),
        Vector(3, 42))
  }

  val v3366: Vertex =
    Vertex(3, 3, 6, 6)

  val v6633: Vertex =
    Vertex(6, 6, 3, 3)

  "A full vertex" can "be contained in another full one" in {
    v3366.isContainedIn(v6633) shouldBe
      true
  }

  "A partial vertex" can "be contained in the same" in {
    Vertex(3, 6).isContainedIn(v6633) shouldBe
      true
  }

  it can "be contained also in a partial one" in {
    Vertex(3, 6).isContainedIn(Vertex(6, 6, 3)) shouldBe
      true
  }

  val v333: Vertex =
    Vertex(3, 3, 3)

  val v33: Vertex =
    Vertex(3, 3)

  val v4444: Vertex =
    Vertex(4, 4, 4, 4)

  val v346: Vertex =
    Vertex(3, 4, 6)

  val v3436: Vertex =
    Vertex(3, 4, 3, 6)

  val v465: Vertex =
    Vertex(4, 6, 5)

  val v46: Vertex =
    Vertex(4, 6)

  val v6: Vertex =
    Vertex(6)

  val v4: Vertex =
    Vertex(4)

  "A full vertex" can "NOT be contained in another full one" in {
    Vertex(3, 6, 3, 6).isContainedIn(v6633) shouldBe
      false
  }

  "A partial vertex" can "NOT be contained in the same" in {
    Vertex(3, 3, 3).isContainedIn(v6633) shouldBe
      false
  }

  "Another partial vertex" can "NOT be contained in another partial one" in {
    v33.isContainedIn(Vertex(6, 6, 3)) shouldBe
      false
  }

  val v84: Vertex =
    Vertex(8, 4)

  val vertices: Vector[Vertex] =
    Vector(
      Vertex(4, 4, 8),
      Vertex(3, 4, 4),
      Vertex(4, 4, 6),
      Vertex(8),
      Vertex(3, 3, 4, 4),
      Vertex(3, 3),
      Vertex(6),
      Vertex(3, 4),
      Vertex(3)
    )

  "A sequence of vertices" can "be reduced to only vertices not contained in others" in {
    vertices.withoutContained shouldBe
      List(
        Vector(3, 3, 4, 4),
        Vector(4, 4, 8),
        Vector(4, 4, 6)
      )
  }

  "The fillables vertices" can "be reduced to only vertices not contained in others" in {
    Vertex.fillables.map(Vertex(_)).toVector.withoutContained shouldBe
      List(
        Vector(3, 3, 3, 3, 3),
        Vector(6, 3, 3, 3),
        Vector(4, 4, 3, 3),
        Vector(4, 3, 4, 3),
        Vector(4, 3, 3, 4),
        Vector(4, 3, 3, 3),
        Vector(3, 6, 3, 3),
        Vector(3, 4, 4, 3),
        Vector(3, 4, 3, 3),
        Vector(12, 4, 3),
        Vector(12, 3, 4),
        Vector(12, 3, 3),
        Vector(6, 6, 3),
        Vector(6, 4, 4),
        Vector(6, 4, 3),
        Vector(6, 3, 6),
        Vector(6, 3, 4),
        Vector(4, 12, 3),
        Vector(4, 6, 4),
        Vector(4, 6, 3),
        Vector(4, 4, 4),
        Vector(3, 12, 3),
        Vector(42, 7),
        Vector(42, 3),
        Vector(24, 8),
        Vector(24, 3),
        Vector(20, 5),
        Vector(20, 4),
        Vector(18, 9),
        Vector(18, 3),
        Vector(15, 10),
        Vector(15, 3),
        Vector(12, 12),
        Vector(12, 6),
        Vector(10, 5),
        Vector(10, 3),
        Vector(9, 3),
        Vector(8, 8),
        Vector(8, 4),
        Vector(8, 3),
        Vector(7, 3),
        Vector(5, 5),
        Vector(5, 4)
      )
  }

  val v44333: Vertex =
    Vertex(4, 4, 3, 3, 3)

  "A vertex" can "be checked if full" in {
    v44333.isFull shouldBe
      true
  }

  "Another vertex" can "be checked if full" in {
    Vertex(4, 3, 4, 3, 3).isFull shouldBe
      true
  }

  "A 6.6.3.3" can "be checked if ordered full" in {
    v6633.isFullMinor shouldBe
      false
  }

  "A 3.3.6.6" can "be checked if ordered full" in {
    v3366.isFullMinor shouldBe
      true
  }

  "A 4.4.3.3" can "be checked if partial" in {
    Vertex(4, 4, 3, 3).isPartial shouldBe
      true
  }

  "A 4.4.4.4" can "be checked if partial" in {
    Vertex(4, 4, 4, 4).isPartial shouldBe
      false
  }

  "A 3.4" can "be checked for the full vertices containing it" in {
    Vertex(3, 4).fullContainers.sortBy(_.toPolygons.map(_.toSides)) shouldBe
      List(
        Vector(3, 3, 3, 4, 4),
        Vector(3, 3, 4, 3, 4),
        Vector(3, 3, 4, 12),
        Vector(3, 4, 3, 12),
        Vector(3, 4, 4, 6),
        Vector(3, 4, 6, 4)
      )
  }

  it can "be checked for fillings" in {
    Vertex(3, 4).fillings.map(_.toPolygons) shouldBe
      List(
        Vector(3, 3, 4),
        Vector(4, 3, 3),
        Vector(3, 4, 3),
        Vector(4, 6),
        Vector(6, 4),
        Vector(12, 3),
        Vector(3, 12)
      )
  }

  "The found 3.4 fillings" must "have the same angle" in {
    Vertex(3, 4).fillings.map(_.alpha).forall(_.toDouble ~= (TAU_4 + TAU_3).toDouble) shouldBe
      true
  }

  "A 4.4" can "be checked for the full vertices containing it" in {
    Vertex(4, 4).fullContainers.sortBy(_.toPolygons.map(_.toSides)) shouldBe
      List(
        Vector(3, 3, 3, 4, 4),
        Vector(3, 4, 4, 6),
        Vector(4, 4, 4, 4)
      )
  }

  it can "be checked for fillings" in {
    Vertex(4, 4).fillings.map(_.toPolygons) shouldBe
      List(
        Vector(3, 3, 3),
        Vector(4, 4),
        Vector(3, 6),
        Vector(6, 3)
      )
  }

  "The found 4.4 fillings" must "have the same angle" in {
    Vertex(4, 4).fillings.map(_.alpha).forall(_.toDouble ~= TAU_2.toDouble) shouldBe
      true
  }

  "A pentagon" can "be checked for the full vertices containing it" in {
    Vertex(5).fullContainers.sortBy(_.toPolygons.map(_.toSides)) shouldBe
      List(
        Vector(4, 5, 20),
        Vector(5, 5, 10)
      )
  }

  "Two vertices" can "be checked if they make a full one" in {
    Vertex(4, 4).isRemainderOf(Vertex(3, 3, 3), v44333) shouldBe
      true
  }

  "Two full vertices 4.4.3.3.3 and 4.3.3.3.4" can "be checked to be the same" in {
    v44333.isSameFull(Vertex(4, 3, 3, 3, 4)) shouldBe
      true
  }

  "Another two full vertices" can "be checked to be the same" in {
    v44333.isSameFull(Vertex(4, 3, 4, 3, 3)) shouldBe
      false
  }

  "A partial and a full vertex" can "be checked to be the same" in {
    Vertex(4, 4, 3, 3).isSameFull(Vertex(4, 4, 3, 3)) shouldBe
      false
  }

}
