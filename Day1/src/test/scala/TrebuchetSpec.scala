import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrebuchetSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

  "Trebuchet" should "match 53894" in {
    Trebuchet.results() should be(53894)
  }

}
