package io.github.scala_tessella.tessella

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.{forAll, propBoolean}
import spire.math.Real
import spire.implicits.partialOrderOps

import io.github.scala_tessella.tessella.Geometry.{PointReal, SimplePolygonReal, LineSegmentReal} // Assuming this is the correct path

object SimplePolygonRealSuite extends Properties("SimplePolygonReal.isSelfIntersecting") {

  // --- Generators ---

  /**
   * Generates Real numbers, including some with fractional parts, within a reasonable range.
   */
  val genRealCoordinate: Gen[Real] = Gen.chooseNum(-100.0, 100.0).map(Real(_))
    .flatMap(r => Gen.chooseNum(-100, 100).map(i => r + Real(i)/Real(100))) // Add some fractional parts for diversity

  /**
   * Generates PointReal instances with coordinates from genRealCoordinate.
   */
  val genPointReal: Gen[PointReal] = for {
    x <- genRealCoordinate
    y <- genRealCoordinate
  } yield PointReal(x, y)

  /**
   * Generates a list of PointReal for polygon construction.
   * For specific shape tests, explicit point construction is often preferred.
   */
  val genPolygonVerticesList: Gen[List[PointReal]] =
    Gen.chooseNum(3, 7).flatMap { n => // Polygons with 3 to 7 vertices
      Gen.listOfN(n, genPointReal)
    }.suchThat(_.size >= 3) // Ensure at least 3 vertices

  // --- Helper for Point Generation ---
  private def createRegularPolygonVertices(cx: Real, cy: Real, radius: Real, numVertices: Int): List[PointReal] = {
    require(numVertices >= 3, "A polygon must have at least 3 vertices.")
    require(radius > Real(0), "Radius must be positive.")
    (0 until numVertices).map { i =>
      val angle = (Real.pi / Real(2)) + (Real(i) * Real(2) * Real.pi / Real(numVertices)) // Start pointing upwards
      PointReal(cx + radius * Real.cos(angle), cy + radius * Real.sin(angle))
    }.toList
  }

  // --- Properties ---

  property("triangles (3 vertices) are not self-intersecting") = forAll(
    genPointReal, genPointReal, genPointReal
  ) { (p1, p2, p3) =>
    // Ensure points are not perfectly collinear for a meaningful geometric triangle,
    // though the method handles collinearity via its n_vertices < 4 check.
    // A simple check for non-collinearity is if ccw is non-zero.
    val isNonCollinear = PointReal.ccw(p1, p2, p3).abs > Real(1e-9) // Using a small epsilon for Real comparison

    val polygon = new SimplePolygonReal(List(p1, p2, p3))
    // The isSelfIntersecting method has: if (n_vertices < 4) return false
    // This property primarily tests this base case.
    (isNonCollinear || true) ==> // Proceed if non-collinear, or always proceed if collinear (still shouldn't intersect)
      !polygon.isSelfIntersecting
  }

  property("squares are not self-intersecting") = forAll(
    genPointReal, // Bottom-left corner
    Gen.posNum[Double].map(d => Real(d + 0.1)).suchThat(_ > Real(0)) // Side length, ensure positive
  ) { (p0, sideLength) =>
    val p1 = PointReal(p0.x, p0.y)
    val p2 = PointReal(p0.x + sideLength, p0.y)
    val p3 = PointReal(p0.x + sideLength, p0.y + sideLength)
    val p4 = PointReal(p0.x, p0.y + sideLength)
    val square = new SimplePolygonReal(List(p1, p2, p3, p4))
    !square.isSelfIntersecting
  }

  property("bowtie/hourglass shape (e.g., (0,0)-(w,h)-(w,0)-(0,h)) is self-intersecting") = forAll(
    genPointReal, // Origin point for the shape
    Gen.posNum[Double].map(d => Real(d + 0.1)).suchThat(_ > Real(0)), // width
    Gen.posNum[Double].map(d => Real(d + 0.1)).suchThat(_ > Real(0))  // height
  ) { (origin, w, h) =>
    val p1 = origin
    val p2 = PointReal(origin.x + w, origin.y + h)
    val p3 = PointReal(origin.x + w, origin.y)
    val p4 = PointReal(origin.x, origin.y + h)
    // Polygon order: p1-p2-p3-p4-p1
    // Edges: p1p2, p2p3, p3p4, p4p1
    // Non-adjacent edges (p1p2, p3p4) or (p2p3, p4p1)
    // Actually, the intersecting pair is p1p2 and p3p4 if ordered (p1,p2,p4,p3)
    // Let's use the standard bowtie point order for clarity: (0,0)-(w,h)-(0,h)-(w,0) leads to no intersection
    // Correct order for intersection: (0,0)-(w,h)-(w,0)-(0,h) which forms edges (0,0)-(w,h), (w,h)-(w,0), (w,0)-(0,h), (0,h)-(0,0).
    // The non-adjacent edges that intersect are ((0,0)-(w,h)) and ((w,0)-(0,h)).
    val bowtie = new SimplePolygonReal(List(p1, p2, p3, p4))
    bowtie.isSelfIntersecting
  }

  property("convex regular pentagons are not self-intersecting") = forAll(
    genRealCoordinate, genRealCoordinate, // Center cx, cy
    Gen.posNum[Double].map(d => Real(d + 0.1)).suchThat(_ > Real(0)) // Scale/radius
  ) { (cx, cy, scale) =>
    val pentagonVertices = createRegularPolygonVertices(cx, cy, scale, 5)
    val pentagon = new SimplePolygonReal(pentagonVertices)
    !pentagon.isSelfIntersecting
  }

  property("pentagram (star shape from 5 points) is self-intersecting") = forAll(
    genRealCoordinate, genRealCoordinate, // Center cx, cy
    Gen.posNum[Double].map(d => Real(d + 0.1)).suchThat(_ > Real(0)) // Scale/radius
  ) { (cx, cy, scale) =>
    val outerVertices = createRegularPolygonVertices(cx, cy, scale, 5)

    // A pentagram connects every second vertex of a regular pentagon
    // Order: P0, P2, P4, P1, P3
    val starVertices = List(
      outerVertices(0),
      outerVertices(2),
      outerVertices(4),
      outerVertices(1),
      outerVertices(3)
    )
    val pentagram = new SimplePolygonReal(starVertices)
    pentagram.isSelfIntersecting
  }

  property("polygon with a 'dent' (collinear points, no crossing) is not self-intersecting") = {
    val p1 = PointReal(Real(0), Real(0))
    val p2 = PointReal(Real(1), Real(0)) // p2 is on segment p1-p3' where p3'=(2,0)
    val p3 = PointReal(Real(2), Real(0))
    val p4 = PointReal(Real(1), Real(1)) // Apex of the dent
    // Polygon is p1-p2-p3-p4-p1. This forms a triangle (p1,p3,p4) with an extra vertex p2 on one side.
    // The implemented method checks (n_vertices < 4) first. This has 4 vertices.
    // Edges: (0,0)-(1,0), (1,0)-(2,0), (2,0)-(1,1), (1,1)-(0,0)
    // No non-adjacent edges should cross.
    val polygon = new SimplePolygonReal(List(p1, p2, p3, p4))
    !polygon.isSelfIntersecting
  }

  property("polygon that backtracks along a line creating self-intersection is self-intersecting") = {
    // p1----p3--p2
    // |    /
    // p5--p4
    val p1 = PointReal(Real(0), Real(0))
    val p2 = PointReal(Real(3), Real(0)) // Long segment initial point
    val p3 = PointReal(Real(1), Real(0)) // Point on the segment p1-p2, creating a backtrack
    val p4 = PointReal(Real(1), Real(1))
    val p5 = PointReal(Real(0), Real(1))

    // Polygon vertices in order: p1, p2, p3, p4, p5
    // Edges:
    // e0: p1-p2 ((0,0)-(3,0))
    // e1: p2-p3 ((3,0)-(1,0))  <-- backtrack
    // e2: p3-p4 ((1,0)-(1,1))
    // e3: p4-p5 ((1,1)-(0,1))
    // e4: p5-p1 ((0,1)-(0,0))
    //
    // Non-adjacent edges to check:
    // e0 vs e2: ((0,0)-(3,0)) intersects ((1,0)-(1,1)) at (1,0).
    // These edges are polygonEdges(0) and polygonEdges(2). Indices 0 and 2 are not adjacent.
    // This should be detected as a self-intersection.
    val polygon = new SimplePolygonReal(List(p1, p2, p3, p4, p5))
    polygon.isSelfIntersecting
  }

  property("polygon with coincident non-adjacent vertices (touching, not crossing) is self-intersecting") = {
    // Creates a polygon that touches itself at a vertex, but isn't a simple adjacency.
    //   p3--p2
    //   | / |
    // p0/p4-p1
    val p0 = PointReal(Real(0), Real(0))
    val p1 = PointReal(Real(2), Real(0))
    val p2 = PointReal(Real(2), Real(1))
    val p3 = PointReal(Real(0), Real(1))
    val p4 = PointReal(Real(1), Real(0)) // This point will be p0 of a segment that touches p2p3

    // Polygon: p0-p1-p2-p4-p3-p0 (forms two squares sharing p4 as a vertex for one, and on edge for other)
    // No, this is not a single loop in the way SimplePolygonReal processes it.
    // Let's make a figure 8 style, where a vertex is shared non-adjacently.
    // p0(0,0)-p1(2,0)-p2(1,1)-p3(2,2)-p4(0,2)-p0(Implicitly via p2(1,1) being revisited)
    // This is like two triangles ((0,0)-(2,0)-(1,1)) and ((1,1)-(2,2)-(0,2)) joined at (1,1)
    val v0 = PointReal(Real(0), Real(0))
    val v1 = PointReal(Real(2), Real(0))
    val v_shared = PointReal(Real(1), Real(1)) // Shared point
    val v3 = PointReal(Real(2), Real(2))
    val v4 = PointReal(Real(0), Real(2))

    val points = List(v0, v1, v_shared, v3, v4, v_shared) // Explicitly repeat shared vertex
    // Edges:
    // e0: v0-v1
    // e1: v1-v_shared
    // e2: v_shared-v3
    // e3: v3-v4
    // e4: v4-v_shared
    // e5: v_shared-v0
    // Check e1 ((2,0)-(1,1)) vs e4 ((0,2)-(1,1)). They touch at (1,1).
    // Indices are 1 and 4. n_vertices = 6. Not adjacent (4 != 1+1, and not 0 & 5). Intersection.
    val polygon = new SimplePolygonReal(points)
    (points.size >=4 ) ==> { // only if it's a valid case for the self-intersection logic (n_vertices >=4)
        polygon.isSelfIntersecting
    }
  }

}