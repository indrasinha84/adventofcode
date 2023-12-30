package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ClumsyCrucibleSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day17/src/test/resources/input/input.XSCORE1.txt"

  "ClumsyCrucible" should "match 102" in {
    ClumsyCrucible.problem1(filePath) should be(102)
  }

  it should "match 69110" in {
    ClumsyCrucible.problem2(filePath) should be(69110)
  }
}
