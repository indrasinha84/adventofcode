import scala.io.Source
import extensions.StringExtensions.*

import scala.util.Using

@main
def Trebuchet(): Int = {
  val filePath = "Day1/src/resources/input/input.XSCORE.txt"
  Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(line => {
        s"${line.lowestAdventDigit}${line.highestAdventDigit}".toInt
      })
      .sum
  }.get
}