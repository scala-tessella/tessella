package io.github.scala_tessella.tessella

import Geometry.*
import Geometry.Radian.TAU
import GeometryBase.*
import RegularPolygon.{Polygon, Vertex}
import Topology.{Degree, Edge, Node}
import conversion.ConverterSVG.Description
import conversion.DOT
import conversion.DOT.toDOT
import conversion.SVGInvalid.*
import utility.Utils.*
import utility.UtilsOption.getDefined

import scala.xml.Elem

/** Descriptive error messages for invalid tiling */
object TilingErrorMessages:

  private def addInfo(kind: String)(info: String): String =
    s"""
       |See $kind:
       |$info""".stripMargin

  private val addDOT: String => String =
    addInfo("DOT")

  private val addSVG: String => String =
    addInfo("SVG")

  private def formatUnion(nodes: List[Node] | List[(Node, Node)]): String =
    val s: String =
      nodes.mkString(", ")
    nodes.size match
      case 1 => s" $s"
      case _ => s"s ($s)"

  private def formatNodes(nodes: List[Node]): String =
    s"node${formatUnion(nodes)}"

  private def formatCouples(nodes: List[(Node, Node)]): String =
    s"couple${formatUnion(nodes)}"

  private def formatEdgeCouples(edges: List[(Edge, Edge)]): String =
    s"couples (${edges.map({ case (e1, e2) => s"(${e1.stringify}, ${e2.stringify})" }).mkString(", ")})"

  extension (graph: Graph)

    /** Error message for disconnected graph, with DOT description */
    def disconnectedErrMsg(intro: String): String =
      val first: List[Edge] =
        graph.disconnectedGraphs.head.graphEdges
      s"$intro disconnected, these sets of nodes are not connected to each other: (${
        graph.graphEdges.disconnectedNodes.map(
          set => set.mkString(", ")
        ).mkString("), (")
      }).${addDOT(graph.toDOT(first.nodes, first))}"

    /** Error message for invalid degree, with DOT description */
    def invalidDegreeErrMsg(f: Degree => Boolean, intro: String): String =
      val invalids: Map[Node, Degree] =
        graph.graphEdges.allDegrees.onlyWithDegree(f)
      val nodes: List[Node] =
        invalids.keys.toList
      s"$intro other nodes, these nodes are not compliant: (${
        invalids.map((node, degree) => s"$node [degree $degree]").mkString(", ")
      }).${addDOT(graph.toDOT(nodes, graph.graphEdges.filter(_.nodes.intersect(nodes).nonEmpty)))}"

    /** Error message for not compacted tiling, with DOT description */
    def interruptedErrMsg: String =
      val nodes: List[Node] =
        graph.diffFromCompacted
      s"""Tiling not compacted, nodes should go uninterrupted from 1 to ${graph.graphEdges.nodes.size}:
         | found ${formatNodes(nodes)}.${addDOT(graph.toDOT(nodes))}""".stripMargin

  extension (tiling: Tiling)

    private def invalidVertexErrMsg(nodes: List[Node], areFull: Boolean): String =
      val vertex: String =
        if areFull then "FullVertex" else "perimeter vertex"
      s"""Tiling must have all ${if areFull then "internal" else "perimeter"} nodes as valid $vertex:
         | found invalid ${formatNodes(nodes)}.${addDOT(tiling.toDOT(nodes))}""".stripMargin

    /** Error message for invalid tiling perimeter vertex, with DOT description */
    def invalidPerimeterVertexErrMsg: String =
      invalidVertexErrMsg(tiling.invalidPerimeterVertices.toList, false)

    /** Error message for invalid tiling full vertex, with DOT description */
    def invalidFullVertexErrMsg: String =
      invalidVertexErrMsg(tiling.invalidFullVertices, true)

    private def addInvalidPerimeterSVG(desc: Description, circles: Elem): String =
      addSVG(
        tiling.invalidPerimeterSVG(
          desc,
          Option(circles)
        )
      )

    private def perimeterNodeFromPoint(point: Point9D, strict: Boolean = false): Node =
      tiling.perimeterCoords
        .find((_, mappedPoint) =>
          if !strict && point.almostEquals(Point9D(0.0, 0.0), ACCURACY) then
            mappedPoint.almostEquals(point, ACCURACY)
          else
            mappedPoint == point
        )
        .map((node, _) => node)
        .get

    /** Error message for invalid tiling vertex coordinates, with SVG description */
    def invalidVertexCoordsErrMsg: String =
      val pointCouples: List[(Point9D, Point9D)] =
        tiling.perimeterPoints2D.almostEqualCouples.toList
      val nodeCouples: List[(Node, Node)] =
        pointCouples.map((p1, p2) => (perimeterNodeFromPoint(p1, true), perimeterNodeFromPoint(p2, true)))
      val svg: String =
        addInvalidPerimeterSVG(
          Description("Invalid touching vertices"),
          intersectionsGroup(pointCouples.map(points => invalidNode(points._1)))
        )
      s"""Tiling must have all perimeter nodes at different cartesian coords:
       | found invalid ${formatCouples(nodeCouples)}.$svg""".stripMargin

    /** Error message for invalid tiling edges intersection, with SVG description */
    def invalidIntersectionErrMsg: String =
      val sideCouples =
        tiling.perimeterSimplePolygon2D.intersectingSides.toList

      def edgeFromSide(side: LineSegment9D): Edge =
        Edge((perimeterNodeFromPoint(side.point1), perimeterNodeFromPoint(side.point2)))

      val edgeCouples: List[(Edge, Edge)] =
        sideCouples.map((s1, s2) => (edgeFromSide(s1), edgeFromSide(s2)))
      val crossings: List[Point9D] =
        edgeCouples
          .map({
            case (edge1, edge2) => List(edge1, edge2).toSegments(tiling.perimeterCoords).toCouple match
              case (f, s) => f.intersection(s)
          })
          .getDefined
      val svg: String =
        addInvalidPerimeterSVG(
          Description("Invalid intersecting edges"),
          intersectionsGroup(crossings.map(invalidNode))
        )
      s"""Tiling must not have intersecting perimeter edges:
         | found invalid ${formatEdgeCouples(edgeCouples)}.$svg""".stripMargin

    /** Error message for invalid grown tiling with exceeding polygons, with SVG description */
    def addExceedingAngleErrMsg(node: Node, vertex: Vertex): String =
      val svg: String =
        addSVG(tiling.invalidTilingSVG(Description(s"Adding exceeding polygons to node $node")))
      val angle: Radian =
        TAU - tiling.perimeterAngles(node)
      s"""Tiling cannot add polygons exceeding the node exterior angle:
         | ${vertex.alpha} is larger than $angle available at node $node.$svg""".stripMargin

    private def addToUnknownNodeErrMsg(node: Node): String =
      val svg: String =
        addSVG(tiling.invalidTilingSVG(Description(s"Adding to unknown node $node")))
      s"""Tiling can add polygons only to perimeter nodes:
         | found unknown node $node.$svg""".stripMargin

    /** Error message for invalid grown tiling from inner node, with SVG description */
    def addToNonPerimeterNodeErrMsg(node: Node): String =
      if tiling.graphEdges.nodes.contains(node) then
        val svg: String =
          addSVG(tiling.invalidTilingSVG(
            Description(s"Adding to inner node $node"),
            Option(invalidNodeGroup(tiling.coords(node)))
          ))
        s"""Tiling can add polygons only to perimeter nodes:
           | found inner node $node.$svg""".stripMargin
      else
        addToUnknownNodeErrMsg(node)

    private def addToUnknownEdgeErrMsg(edge: Edge): String =
      val svg: String =
        addSVG(tiling.invalidTilingSVG(
          Description(s"Adding to unknown edge ${edge.stringify}"),
          if edge.nodes.diff(tiling.graphEdges.nodes).isEmpty then
            Option(invalidEdgeGroup(edge.toSegment(tiling.coords)))
          else
            None
        ))
      s"""Tiling can add polygons only to perimeter edges:
         | found unknown edge ${edge.stringify}.$svg""".stripMargin

    /** Error message for invalid grown tiling from inner edge, with SVG description */
    def addToNonPerimeterEdgeErrMsg(edge: Edge): String =
      if tiling.edges.contains(edge) then
        val svg: String =
          addSVG(tiling.invalidTilingSVG(
            Description(s"Adding to inner edge ${edge.stringify}"),
            Option(invalidEdgeGroup(edge.toSegment(tiling.coords), 3))
          ))
        s"""Tiling can add polygons only to perimeter edges:
           | found inner edge ${edge.stringify}.$svg""".stripMargin
      else
        addToUnknownEdgeErrMsg(edge)

    /** Error message for invalid grown tiling with no edges fillable by specific polygon, with SVG description */
    def noFillablePerimeterEdgesErrMsg(polygon: Polygon, step: Int): String =
      val svg: String =
        addSVG(tiling.invalidTilingSVG(
          Description(s"No more space to add pgon-$polygon after $step steps")
        ))
      s"""Tiling cannot be grown after adding $step * pgon-$polygon,
         | no more edges fillable$svg""".stripMargin
