package problem

import scala.io.Source
import scala.util.Using

object Day3Problem {
  def problem1(filePath: String): Iterator[String] = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
    }.get
  }

  def problem2(filePath: String): Iterator[String] = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
    }.get
  }
}