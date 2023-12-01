import scala.io.Source
import extensions.StringExtensions.*

import scala.util.Using

@main
def main(): Unit = {
  val filePath = "Day1/src/resources/input/input.XSCORE.txt"
  Using(Source.fromFile(filePath)) { file =>
    val result = file
      .getLines()
      .map(line => s"${line.lowestAdventDigit}${line.highestAdventDigit}".toInt)
      .sum
    println(result)
  }
}