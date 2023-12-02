package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CubeConundrumSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day2/src/test/resources/input/input.XSCORE.txt"

  "CubeConundrum" should "match 2810" in {
    CubeConundrum.problem1(filePath) should be(2810)
  }

  it should "match 69110" in {
    CubeConundrum.problem2(filePath) should be(69110)
  }
}
