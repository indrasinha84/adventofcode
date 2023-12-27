package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CamelCardsSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day7/src/test/resources/input/input.XSCORE.txt"

  "CamelCards" should "match 247961593" in {
    CamelCards.problem1(filePath) should be(247961593)
  }

  it should "match 248750699" in {
    CamelCards.problem2(filePath) should be(248750699)
  }
}
