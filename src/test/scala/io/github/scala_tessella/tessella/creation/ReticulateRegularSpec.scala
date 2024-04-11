package io.github.scala_tessella.tessella
package creation

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class ReticulateRegularSpec extends AnyFlatSpec with Helper with should.Matchers {

  "A square net" can "be generated with width 1 and height 1" in {
    Tiling.squareNet(1, 1).map(_.toString) shouldEqual
      Right("Tiling(1--2, 1--3, 2--4, 3--4)")
  }

  it can "NOT be generated with width 0" in {
    Tiling.squareNet(0, 1) shouldEqual
      Left("Width should be greater than 0")
  }

  it can "NOT be generated with height 0" in {
    Tiling.squareNet(1, 0) shouldEqual
      Left("Height should be greater than 0")
  }

  "A triangular net" can "be generated with width 2 and height 1" in {
    Tiling.triangleNet(2, 1).map(_.toString) shouldEqual
      Right("Tiling(1--2, 1--3, 1--4, 2--4, 3--4)")
  }

  it can "NOT be generated with width not even" in {
    Tiling.triangleNet(3, 1) shouldEqual
      Left("Width should be even and greater than 0")
  }

  it can "NOT be generated with width 0" in {
    Tiling.triangleNet(0, 1) shouldEqual
      Left("Width should be even and greater than 0")
  }

  it can "NOT be generated with height 0" in {
    Tiling.triangleNet(2, 0) shouldEqual
      Left("Height should be greater than 0")
  }

  "An hexagonal net" can "be generated with width 1 and height 1" in {
    Tiling.hexagonNet(1, 1).map(_.toString) shouldEqual
      Right("Tiling(1--2, 1--4, 2--3, 3--6, 4--5, 5--6)")
  }

  it can "NOT be generated with width 0" in {
    Tiling.hexagonNet(0, 1) shouldEqual
      Left("Width should be greater than 0")
  }

  it can "NOT be generated with height 0" in {
    Tiling.hexagonNet(1, 0) shouldEqual
      Left("Height should be greater than 0")
  }

}
