package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HauntedWastelandSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day8/src/test/resources/input/input.XSCORE.txt"

  "HauntedWasteland" should "match 22411" in {
    HauntedWasteland.problem1(filePath) should be(22411)
  }

  it should "match 11188774513823" in {
    HauntedWasteland.problem2(filePath) should be(11188774513823L)
  }
}
