package io.github.scala_tessella.tessella // Or your actual package

import io.github.scala_tessella.tessella.Topology.{Edge, Node}
import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.util.Random
import scala.util.Try // Add Try if not already in scope

// --- JMH Annotations ---
// OutputTimeUnit: Sets the time unit for the results.
// BenchmarkMode: Mode for benchmarking (Throughput, AverageTime, SampleTime, SingleShotTime). AverageTime is good for comparing specific operations.
// State: Scope of the state object (Benchmark, Group, Thread). Benchmark means one instance per full benchmark run.
// Fork: How many times to fork the JVM. Forking isolates benchmark runs.
// Warmup: Configuration for warmup iterations before actual measurement.
// Measurement: Configuration for actual measurement iterations.
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Benchmark)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G")) // Example JVM args, adjust as needed
@Warmup(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 20, time = 1, timeUnit = TimeUnit.SECONDS)
class TopologyBenchmarkWithBFS {

  // --- State for the Benchmark ---
  @Param(Array("4", "8", "12", "16", "20")) // Approx number of polygons for square side
  var side: Int = 0

  // This variable will hold the graph edges.
  // It's good practice to initialize it in a @Setup method.
  var graphEdges: List[Edge] = _
  var rootNode: Node = _
  var targetNodeOption: Option[Node] = None
  var targetNodePresent: Node = _

  val patternsVector: Vector[Int => Tiling] =
    Vector(
      side => Tiling.pattern_333333_33336_3366_666(side, side).toOption.get,
      side => Tiling.pattern_333333_3366_2x666(side, side).toOption.get,
      side => Tiling.pattern_333333_2x3366_666(side, side).toOption.get,
      side => Tiling.pattern_33344_33434_4444(side, side).toOption.get,
      side => Tiling.pattern_33434(side, side).toOption.get,
      side => Tiling.pattern_488(side, side).toOption.get,
      side => Tiling.pattern_666(side, side).toOption.get,
      side => Tiling.pattern_3636_3366(side, side).toOption.get,
    )

  @Setup(Level.Trial) // Run once per benchmark trial (for each @Param combination)
  def setupTilingGraph(): Unit =
    val random = new Random(42) // Seed for reproducibility
    val pattern = random.between(0, patternsVector.size - 1)
    val tiling = patternsVector(pattern)(side)

    graphEdges = tiling.graphEdges

    val z = tiling.graphNodes
    rootNode = Node(1)

    if z.nonEmpty then
      targetNodePresent = graphEdges.last.greaterNode // Ensure this target exists
      targetNodeOption = Some(targetNodePresent)
    else
    // Handle empty graph case if necessary, though @Param starts at 100 nodes
      targetNodePresent = Node(2) // Placeholder
      targetNodeOption = None

  // --- Benchmarks ---

  @Benchmark
  def bfs_original_noTarget(): List[Node] = {
    if (graphEdges.isEmpty) Nil else graphEdges.bfs(rootNode, None)
  }

  @Benchmark
  def bfsFast_improved_noTarget(): List[Node] = {
    if (graphEdges.isEmpty) Nil else graphEdges.bfsFast(rootNode, None)
  }

  @Benchmark
  def bfs_original_withTarget(): List[Node] = {
    if (graphEdges.isEmpty) Nil else graphEdges.bfs(rootNode, targetNodeOption)
  }

  @Benchmark
  def bfsFast_improved_withTarget(): List[Node] = {
    if (graphEdges.isEmpty) Nil else graphEdges.bfsFast(rootNode, targetNodeOption)
  }
}