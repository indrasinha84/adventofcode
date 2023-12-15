package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LensLibrarySpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day15/src/test/resources/input/input.XSCORE.txt"

  "LensLibrary" should "match 514281" in {
    LensLibrary.problem1(filePath) should be(514281)
  }

  it should "match 244199" in {
    LensLibrary.problem2(filePath) should be(244199)
  }
}
