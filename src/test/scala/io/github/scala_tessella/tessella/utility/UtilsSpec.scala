package io.github.scala_tessella.tessella
package utility

import Utils.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class UtilsSpec extends AnyFlatSpec with should.Matchers {

  val aMap: Map[Int, String] =
    Map(
      1 -> "banana",
      2 -> "apple",
      3 -> "orange"
    )

  "A Map" can "be inverted" in {
    aMap.invert shouldBe
      Map(
        "banana" -> 1,
        "apple" -> 2,
        "orange" -> 3
      )
  }

  it can "have its values modified" in {
    aMap.mapValues2(_.length) shouldBe
      Map(
        1 -> 6,
        2 -> 5,
        3 -> 6
      )
  }

  val aMapWithRepeatedValues: Map[Int, String] =
    aMap ++ Map(
      4 -> "banana",
      5 -> "banana",
      6 -> "orange"
    )

  val aMapWithGroupedValues: Map[String, Iterable[Int]] =
    Map(
      "banana" -> List(5, 1, 4),
      "orange" -> List(6, 3),
      "apple" -> List(2)
    )

  it can "be grouped by values" in {
    aMapWithRepeatedValues.groupByValues shouldBe
      aMapWithGroupedValues
  }

  "A map with iterable values" can "be mapped by value" in {
    aMapWithGroupedValues.mapByValue shouldBe
      aMapWithRepeatedValues
  }

  "A sequence" can "be reduced to the couple of the first two elements" in {
    Seq(4, 5, 6).toCouple shouldBe
      (4, 5)
  }

  it must "fail to be reduced to couple when size is 1" in {
    val caught: IndexOutOfBoundsException =
      intercept[IndexOutOfBoundsException] { Seq(4).toCouple}
    caught.getMessage shouldBe "1"
  }

  it must "fail to be reduced to couple when empty" in {
    val caught: IndexOutOfBoundsException =
      intercept[IndexOutOfBoundsException] { Seq().toCouple }
    caught.getMessage shouldBe "0"
  }

  val aSequence: Seq[Int] =
    Seq(1, 2, 1, 1, 3, 4, 5, 2, 4, 6)

  it can "be grouped by size" in {
    aSequence.groupBySize shouldBe
      Map(
        1 -> 3,
        2 -> 2,
        3 -> 1,
        4 -> 2,
        5 -> 1,
        6 -> 1
      )
  }

  it can "be filtered only with unique elements" in {
    aSequence.filterUnique shouldBe
      Set(5, 6, 3)
  }

  it can "be filtered only with not unique elements" in {
    aSequence.filterNotUnique shouldBe
      Set(1, 2, 4)
  }

  it can "be converted into a map with values modified" in {
    Seq(1, 2, 3).toMap2(_ * 3) shouldBe
      Map(
        1 -> 3,
        2 -> 6,
        3 -> 9
      )
  }

  "A list of elements" can "be grouped into separate groups where all elements are connected by a function" in {
    val absoluteDifferenceLessThanTwo: (Int, Int) => Boolean =
      (x, y) => Math.abs(x - y) < 2
    List(1, 2, 3, 5).groupConnected(absoluteDifferenceLessThanTwo) shouldBe
      List(List(5), List(1, 2, 3))
  }

}
