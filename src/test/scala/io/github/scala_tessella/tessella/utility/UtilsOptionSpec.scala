package io.github.scala_tessella.tessella
package utility

import utility.UtilsOption.{equalsWhereDefined, sequence}

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class UtilsOptionSpec extends AnyFlatSpec with should.Matchers {

  def optionize(s: String): Vector[Option[Char]] =
    s.map({
      case '*' => None
      case c   => Option(c)
    }).toVector

  val RING: Vector[Option[Char]] =
    optionize("RING")

  val R_NG: Vector[Option[Char]] =
    optionize("R*NG")

  "A vector of optional elements all defined" can "be created" in {
    RING shouldBe
      Vector(Option('R'), Option('I'), Option('N'), Option('G'))
  }

  it can "be transformed in a defined optional vector of elements" in {
    RING.sequence shouldBe
      Option(Vector('R', 'I', 'N', 'G'))
  }

  "A vector of optional elements not all defined" can "be created" in {
    R_NG shouldBe
      Vector(Option('R'), None, Option('N'), Option('G'))
  }

  it can "be trasformed in a undefined optional vector of elements" in {
    R_NG.sequence shouldBe
      None
  }

  "A vector of optional elements" can "be equal to itself where defined" in {
    R_NG.equalsWhereDefined(R_NG) shouldBe
      true
  }

  it can "be equal to another vector where defined" in {
    R_NG.equalsWhereDefined(optionize("R**G")) shouldBe
      true
  }

  it can "be NOT equal to another vector where defined" in {
    R_NG.equalsWhereDefined(optionize("P*NG")) shouldBe
      false
    optionize("****").equalsWhereDefined(optionize("WHAT")) shouldBe
      true
  }

  "A vector of all undefined optional elements" can "be equal to a vector of all defined optional elements" in {
    optionize("****").equalsWhereDefined(optionize("WHAT")) shouldBe
      true
  }

}
