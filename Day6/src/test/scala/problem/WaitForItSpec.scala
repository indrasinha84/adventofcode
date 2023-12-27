package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WaitForItSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day6/src/test/resources/input/input.XSCORE.txt"

  "WaitForIt" should "match 138915" in {
    WaitForIt.problem1(filePath) should be(138915L)
  }

  it should "match 27340847" in {
    WaitForIt.problem2(filePath) should be(27340847L)
  }
}
