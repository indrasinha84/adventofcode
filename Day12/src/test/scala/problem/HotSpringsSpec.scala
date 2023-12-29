package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NewProblemSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day2/src/test/resources/input/input.XSCORE.txt"

  "NewProblem" should "match 2810" in {
    Day3Problem.problem1(filePath) should be(2810)
  }

  it should "match 69110" in {
    Day3Problem.problem2(filePath) should be(69110)
  }
}
