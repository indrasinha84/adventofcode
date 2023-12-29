package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HotSpringsSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day12/src/test/resources/input/input.XSCORE.txt"

  "HotSprings" should "match 6488" in {
    HotSprings.problem1(filePath) should be(6488)
  }

  it should "match 815364548481" in {
    HotSprings.problem2(filePath) should be(815364548481L)
  }
}
