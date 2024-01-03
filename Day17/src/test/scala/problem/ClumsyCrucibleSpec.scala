package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ClumsyCrucibleSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day17/src/test/resources/input/input.XSCORE.txt"

  "ClumsyCrucible" should "match 1256" in {
    ClumsyCrucible.problem1(filePath) should be(1256)
  }

  it should "match 1382" in {
    ClumsyCrucible.problem2(filePath) should be(1382)
  }
}
