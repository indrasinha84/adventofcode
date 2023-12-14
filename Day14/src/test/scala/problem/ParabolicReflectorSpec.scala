package problem

import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit

class ParabolicReflectorSpec extends AnyFlatSpec with GivenWhenThen with Matchers {

//  val filePath = "Day14/src/test/resources/input/input.XSCORE.txt"
  val filePath = "Day14/src/test/resources/input/input.XSCOREsmall.txt"

  "ParabolicReflector" should "match 2810" in {
    ParabolicReflector.problem1(filePath) should be(109466)
  }

  it should "match 631" in {
    val start = System.currentTimeMillis()
    val result = ParabolicReflector.problem2(filePath)
    result should not equal null
    println(s"Result is $result")
    val duration = System.currentTimeMillis() - start
    println(s"Completed in ${
      String.format("%d min, %d sec",
        TimeUnit.MILLISECONDS.toMinutes(duration),
        TimeUnit.MILLISECONDS.toSeconds(duration) -
          TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(duration))
      )}")
  }
}
