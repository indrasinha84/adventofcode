package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TheFloorWillBeLavaSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day16/src/test/resources/input/input.XSCORE.txt"

  "TheFloorWillBeLava" should "match 8901" in {
    TheFloorWillBeLava.problem1(filePath) should be(8901)
  }

  it should "match 69110" in {
    TheFloorWillBeLava.problem2(filePath) should be(69110)
  }
}
