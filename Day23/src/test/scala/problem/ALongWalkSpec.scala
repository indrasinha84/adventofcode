package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ALongWalkSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day23/src/test/resources/input/input.XSCORE.txt"

  "ALongWalk" should "match 1998" in {
    ALongWalk.problem1(filePath) should be(1998)
  }

  it should "match 69110" in {
    ALongWalk.problem2(filePath) should be(69110)
  }
}
