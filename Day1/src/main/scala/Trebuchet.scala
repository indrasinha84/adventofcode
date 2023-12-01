import scala.io.Source
import extensions.StringExtensions.*

import scala.util.Using

object Trebuchet {
  def results(filePath: String): Int = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(line => {
          s"${line.lowestAdventDigit}${line.highestAdventDigit}".toInt
        })
        .sum
    }.get
  }
}