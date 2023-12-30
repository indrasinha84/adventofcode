package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PulsePropagationSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day20/src/test/resources/input/input.XSCORE.txt"

  "PulsePropagation" should "match 912199500" in {
    PulsePropagation.problem1(filePath) should be(912199500)
  }

  it should "match 69110" in {
    PulsePropagation.problem2(filePath) should be(69110)
  }
}
