package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import problem.Trebuchet

class TrebuchetSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day1/src/test/resources/input/input.XSCORE.txt"

  "Trebuchet" should "match 53894" in {
    Trebuchet.problem1(filePath) should be(53651)
  }

  it should "match 538941" in {
    Trebuchet.problem2(filePath) should be(53894)
  }
}
