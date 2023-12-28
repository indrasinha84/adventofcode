package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MirageMaintenanceSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day9/src/test/resources/input/input.XSCORE.txt"

  "MirageMaintenance" should "match 2105961943" in {
    MirageMaintenance.problem1(filePath) should be(2105961943L)
  }

  it should "match 1019" in {
    MirageMaintenance.problem2(filePath) should be(1019L)
  }
}
