package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SandSlabsSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day22/src/test/resources/input/input.XSCORE.txt"

  "SandSlabs" should "match 418" in {
    SandSlabs.problem1(filePath) should be(418)
  }

  it should "match 70702" in {
    SandSlabs.problem2(filePath) should be(70702)
  }
}
