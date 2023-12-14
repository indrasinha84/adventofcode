package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParabolicReflectorSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day14/src/test/resources/input/input.XSCORE.txt"

  "ParabolicReflector" should "match 2810" in {
    ParabolicReflector.problem1(filePath) should be(109466)
  }

  it should "match 69110" in {
    ParabolicReflector.problem2(filePath) should be(69110)
  }
}
