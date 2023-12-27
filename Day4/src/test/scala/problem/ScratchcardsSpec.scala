package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ScratchcardsSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day4/src/test/resources/input/input.XSCORE.txt"

  "Scratchcards" should "match 25231" in {
    Scratchcards.problem1(filePath) should be(25231)
  }

  it should "match 9721255" in {
    Scratchcards.problem2(filePath) should be(9721255)
  }
}
