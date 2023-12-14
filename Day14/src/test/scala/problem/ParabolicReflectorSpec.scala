package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit

class ParabolicReflectorSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day14/src/test/resources/input/input.XSCORE.txt"

  "ParabolicReflector" should "match 2810" in {
    ParabolicReflector.problem1(filePath) should be(109466)
  }

  it should "match 94585" in {
    ParabolicReflector.problem2(filePath) should be(94585)
  }
}
