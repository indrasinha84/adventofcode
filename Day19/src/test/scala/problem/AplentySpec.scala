package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AplentySpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day19/src/test/resources/input/input.XSCORE.txt"

  "Aplenty" should "match 346230" in {
    Aplenty.problem1(filePath) should be(346230)
  }

  it should "match 124693661917133" in {
    Aplenty.problem2(filePath) should be(124693661917133L)
  }
}
