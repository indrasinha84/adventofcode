package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StepCounterSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day21/src/test/resources/input/input.XSCORE1.txt"

  "StepCounter" should "match 3632" in {
    StepCounter.problem1(filePath) should be(3632)
  }

  it should "match 69110" in {
    StepCounter.problem2(filePath) should be(69110)
  }
}
