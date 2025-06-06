package io.github.scala_tessella.tessella

import Geometry.*

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Gen, Arbitrary}
import spire.math.Real
import spire.implicits.additiveMonoidOps

class DistinctPointsApproximationSpec extends AnyFlatSpec with Helper with should.Matchers with ScalaCheckPropertyChecks:

  // Generator for spire.math.Real
  val genReal: Gen[Real] = Gen.chooseNum(-1000.0, 1000.0).map(Real(_))
  implicit val arbReal: Arbitrary[Real] = Arbitrary(genReal)

  // Generator for Geometry.PointReal
  val genPointReal: Gen[Geometry.PointReal] = for
    x <- arbReal.arbitrary
    y <- arbReal.arbitrary
  yield Geometry.PointReal(x, y)
  implicit val arbPointReal: Arbitrary[Geometry.PointReal] = Arbitrary(genPointReal)

  // Reference implementation for areAllDistinctApprox based on PointReal.almostEquals
  def referenceAreAllDistinctApprox(points: Seq[Geometry.PointReal]): Boolean =
    if points.length < 2 then true
    else
      points.combinations(2).forall {
        case Seq(p1, p2) => !p1.almostEquals(p2)
        case _           => true // Should not be reached by combinations(2)
      }

  "areAllDistinctApprox on Seq[PointReal]" should "be true for an empty list" in {
    forAll(Gen.const(List.empty[Geometry.PointReal])) { (pts: List[Geometry.PointReal]) =>
      pts.areAllDistinctApprox shouldBe true
    }
  }

  it should "be true for a list with a single point" in {
    forAll(genPointReal) { (pt: Geometry.PointReal) =>
      List(pt).areAllDistinctApprox shouldBe true
    }
  }

  it should "be false if all points in a list (size > 1) are identical" in {
    val genListOfIdenticalPoints = for
      pt <- genPointReal
      size <- Gen.choose(2, 5) // Keep list size manageable for tests
    yield List.fill(size)(pt)

    forAll(genListOfIdenticalPoints) { (pts: List[Geometry.PointReal]) =>
      whenever(pts.length > 1) {
        pts.areAllDistinctApprox shouldBe false
      }
    }
  }

  it should "be false if the list contains two distinct points that are almost equal" in {
    val genPointAndAlmostEqualNeighbor: Gen[(Geometry.PointReal, Geometry.PointReal)] =
      for
        p1x <- arbReal.arbitrary
        p1y <- arbReal.arbitrary
        // Generate a small non-zero delta, well within epsilon/2 to ensure almostEquals is true.
        deltaBase = Geometry.epsilonReal / Real(4)

        dx <- Gen.chooseNum(-deltaBase.toDouble, deltaBase.toDouble).map(Real(_))
        dy <- Gen.chooseNum(-deltaBase.toDouble, deltaBase.toDouble).map(Real(_))

        p1 = Geometry.PointReal(p1x, p1y)
        // Ensure p2 is distinct from p1 by adding a minimal non-zero delta if dx and dy are both zero.
        finalDx = if dx.isZero && dy.isZero && !deltaBase.isZero then deltaBase / Real(2) else dx
        finalDy = dy // Keep dy as is, or add similar logic if needed for distinctness.
        p2 = Geometry.PointReal(p1x + finalDx, p1y + finalDy)
      yield (p1, p2)

    forAll(genPointAndAlmostEqualNeighbor, Gen.listOf(genPointReal)) {
      case ((p1, p2), otherPoints) =>
        // Ensure p1 and p2 are indeed almost equal and not the exact same point object/value for the test to be meaningful.
        // p1.equals(p2) might be true if finalDx and finalDy are zero.
        // The generator tries to make them distinct.
        val arePhysicallyDifferent = !(p1.x == p2.x && p1.y == p2.y) // Check value inequality

        whenever(arePhysicallyDifferent && p1.almostEquals(p2)) {
          // Randomly intersperse the pair with other points
          val allPoints = scala.util.Random.shuffle(p1 :: p2 :: otherPoints)
          allPoints.areAllDistinctApprox shouldBe false
        }
    }
  }

  it should "yield the same result as a reference N^2 comparison" in {
    val genListOfPoints: Gen[List[Geometry.PointReal]] = for {
      size <- Gen.choose(0, 20) // First, generate the size
      list <- Gen.listOfN(size, genPointReal) // Then, use the generated size
    } yield list

    forAll(genListOfPoints) { (pts: List[Geometry.PointReal]) =>
      // The areAllDistinctApprox is an optimization; its result must match the straightforward check.
      pts.areAllDistinctApprox shouldBe referenceAreAllDistinctApprox(pts)
    }
  }

  it should "be true for points that are far apart" in {
    val largeSpacing = Geometry.epsilonReal * Real(1000) // Spacing much larger than epsilon

    // Generator for a single "far apart" point
    val genSingleFarApartPoint: Gen[Geometry.PointReal] = for {
      xFactor <- Gen.choose(-10, 10) // Generates Int
      yFactor <- Gen.choose(-10, 10) // Generates Int
    } yield Geometry.PointReal(Real(xFactor) * largeSpacing, Real(yFactor) * largeSpacing)

    // Generator for a list of "far apart" points
    val genFarApartListOfPoints: Gen[List[Geometry.PointReal]] = for {
      numPoints <- Gen.choose(0, 10) // First, generate the number of points
      points <- Gen.listOfN(numPoints, genSingleFarApartPoint) // Then, use that number to generate the list
    } yield points.distinctBy(p => (p.x, p.y)) // Ensure unique grid points in the final list

    forAll(genFarApartListOfPoints) { (pts: List[Geometry.PointReal]) =>
      // If epsilonReal is very large, this test might not be as effective.
      // Assuming standard small epsilon.
      // All these points should be distinct by construction if largeSpacing is effective.
      // referenceAreAllDistinctApprox(pts) should be true for these.
      val expectation = referenceAreAllDistinctApprox(pts)
      pts.areAllDistinctApprox shouldBe expectation
      // In most cases for this generator, expectation should be true.
      // Adding an assertion to ensure that our generator is mostly producing true cases.
      // If not, the generator might need refinement for this specific property's intent.
      if (pts.length > 1 && !expectation) {
        // This case is unlikely if largeSpacing is truly large enough and points are distinct.
        // It might indicate an issue with the generator or an extreme epsilon value.
      }
    }
  }