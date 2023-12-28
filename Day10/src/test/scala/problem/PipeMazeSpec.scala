package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PipeMazeSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day10/src/test/resources/input/input.XSCORE.txt"

  "PipeMaze" should "match 6979" in {
    PipeMaze.problem1(filePath) should be(6979L)
  }

  it should "match 69110" in {
    PipeMaze.problem2(filePath) should be(69110L)
  }
}
