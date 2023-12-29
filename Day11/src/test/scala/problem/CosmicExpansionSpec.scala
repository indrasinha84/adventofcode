package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CosmicExpansionSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day11/src/test/resources/input/input.XSCORE.txt"

  "CosmicExpansion" should "match 9543156" in {
    CosmicExpansion.problem1(filePath) should be(9543156)
  }

  it should "match 625243292686" in {
    CosmicExpansion.problem2(filePath) should be(625243292686L)
  }
}
