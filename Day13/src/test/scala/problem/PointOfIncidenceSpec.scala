package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PointOfIncidenceSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day13/src/test/resources/input/input.XSCORE.txt"

  "PointOfIncidence" should "match 33520" in {
    PointOfIncidence.problem1(filePath) should be(33520)
  }

  it should "match 69110" in {
    PointOfIncidence.problem2(filePath) should be(69110)
  }
}
