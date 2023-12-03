package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GearRatiosSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day3/src/test/resources/input/input.XSCORE.txt"

  "GearRatios" should "match 556367" in {
    GearRatios.problem1(filePath) should be(556367)
  }
//
//  it should "match 69110" in {
//    Day3Problem.problem2(filePath) should be(69110)
//  }
}
