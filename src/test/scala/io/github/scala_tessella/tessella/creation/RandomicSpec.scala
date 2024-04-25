package io.github.scala_tessella.tessella
package creation

import Randomic.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RandomicSpec extends AnyFlatSpec with Helper with should.Matchers{

  "A random polygon" can "be added to an empty tiling" in {
    Tiling.empty.randomStep().get.countPolygons shouldBe
      1
  }
  
  it can "be added to a tiling formed by only a polygon" in {
    triangle.randomStep().get.countPolygons shouldBe
      2
  }
  
  "Two random polygons" can "be added to an empty tiling" in {
    Tiling.empty.randomSteps(2).get.countPolygons shouldBe
      2
  }


}
