package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IfYouGiveASeedAFertilizerSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day5/src/test/resources/input/input.XSCORE.txt"

  "IfYouGiveASeedAFertilizer" should "match 196167384" in {
    IfYouGiveASeedAFertilizer.problem1(filePath) should be(196167384L)
  }

  it should "match 69110" in {
    IfYouGiveASeedAFertilizer.problem2(filePath) should be(125742456L)
  }
}
