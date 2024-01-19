package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NeverTellMeTheOddsSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day24/src/test/resources/input/input.XSCORE.txt"

  "NeverTellMeTheOdds" should "match 14046" in {
    NeverTellMeTheOdds.problem1(filePath) should be(14046)
  }

  it should "match 69110" in {
    NeverTellMeTheOdds.problem2(filePath) should be(69110L)
  }
}
