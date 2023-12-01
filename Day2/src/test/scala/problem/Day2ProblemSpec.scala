import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import problem.Day2Problem

class Day2ProblemSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day1/src/test/resources/input/input.XSCORE.txt"

  "Trebuchet" should "match 53894" in {
    Day2Problem.results(filePath) should be(53894)
  }
}
