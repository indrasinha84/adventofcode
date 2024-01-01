package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SnowverloadSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day25/src/test/resources/input/input.XSCORE.txt"

  "Snowverload" should "match 2810" in {
    Snowverload.problem1(filePath) should be(2810)
  }

  it should "match 69110" in {
    Snowverload.problem2(filePath) should be(69110)
  }
}
