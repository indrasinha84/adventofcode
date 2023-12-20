package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LavaductLagoonSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day18/src/test/resources/input/input.XSCORE.txt"

  "LavaductLagoon" should "match 59560" in {
    LavaductLagoon.problem1(filePath) should be(59560)
  }

  it should "match 69110" in {
    LavaductLagoon.problem2(filePath) should be(69110)
  }
}
