import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrebuchetSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  val filePath = "Day1/src/test/resources/input/input.XSCORE.txt"

  "Trebuchet" should "match 53894" in {
    Trebuchet.results(filePath) should be(53894)
  }
}
