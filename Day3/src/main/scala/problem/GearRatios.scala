package problem

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object GearRatios {

""
  private def isValidPartNumber(base: Array[Array[Char]], start: (Int, Int), end: (Int, Int)): Boolean = {
    if (start._2 > 0) {
      val valueToCheck = base(start._1)(start._2 - 1)
      if (!valueToCheck.isDigit && valueToCheck != '.') {
        return true
      }
    }
    if (end._2 < base(start._1).length - 1) {
      val valueToCheck = base(start._1)(end._2 + 1)
      if (!valueToCheck.isDigit && valueToCheck != '.') {
        return true
      }
    }
    for j <- 0.max(start._2 - 1) to (base(start._1).length - 1).min(end._2 + 1) do {
      if (start._1 > 0) {
        val valueToCheck1 = base(start._1 - 1)(j)
        if (!valueToCheck1.isDigit && valueToCheck1 != '.') {
          return true
        }
      }
      if (start._1 < base.length - 1) {
        val valueToCheck2 = base(start._1 + 1)(j)
        if (!valueToCheck2.isDigit && valueToCheck2 != '.') {
          return true
        }
      }
    }
    false
  }

  def problem1(filePath: String): Int = {
    val result = mutable.ListBuffer.empty[Int]
    Using(Source.fromFile(filePath)) { file =>
      var currentNumber = ""
      var start = (-1, -1)
      var end = (-1, -1)
      val base = file
        .getLines()
        .map(_.toCharArray)
        .toArray
      for i <- base.indices do {
        for j <- base(i).indices do {
          val c = base(i)(j)
          if (c.isDigit) {
            if (currentNumber.isBlank) {
              start = (i, j)
            }
            currentNumber += c

          }
          if (!currentNumber.isBlank && (j == base(i).length - 1 || !base(i)(j + 1).isDigit)) {
            end = (i, j)
            if (isValidPartNumber(base, start, end)) {
              result += currentNumber.toInt
            }
            currentNumber = ""
          }
        }
      }

      result
    }.get
    result.sum
  }

  def problem2(filePath: String): Iterator[String] = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
    }.get
  }


}