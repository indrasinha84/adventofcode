package problem

import utils.{findValidNeighboursF, isValidIndexF}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

final case class Position(x: Int, y: Int)

object GearRatios {


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

  def getValidPartNumbers(filePath: String): Seq[(Int, (Int, Int), (Int, Int))] = {
    val result = mutable.ListBuffer.empty[(Int, (Int, Int), (Int, Int))]
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
              result += ((currentNumber.toInt, start, end))
            }
            currentNumber = ""
          }
        }
      }
      result.toSeq
    }.get
  }

  def problem1(filePath: String): Int = {
    val result = getValidPartNumbers(filePath)
    result.map(_._1).sum
  }

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray)
      .toArray
  }.get

  def problem1SlowSolution(filePath: String): Int = {
    val input = readFile(filePath)
    val isValidIndex = isValidIndexF(input.length, input.head.length)
    val findValidNeighbours = findValidNeighboursF(isValidIndex)
    val splitChars = input.flatMap(_.filter(c => !c.isDigit))
    input.indices.map(row => {
      val validPositions = input(row).indices.map(col => (row, col))
        .filter({ case (r, c) => input(r)(c).isDigit &&
          findValidNeighbours((r, c), true)
            .exists({ case (r, c) => !input(r)(c).isDigit && input(r)(c) != '.' })
        }).toSet
      val inpText = input(row).mkString
      inpText.split(splitChars).filter(!_.isBlank)
        .foldLeft((0, Seq.empty[(Int, Set[(Int, Int)])]))((sum, elem) => {
          val startOfNumber = inpText.indexOf(elem, sum._1)
          val positionsOfNumber = (startOfNumber until startOfNumber + elem.length).map(c => (row, c)).toSet
          val newMap = sum._2 :+ (elem.toInt, positionsOfNumber)
          (startOfNumber + elem.length, newMap)
        })._2.filter(d => {
          (d._2 intersect validPositions).nonEmpty
        })
    }
    ).flatMap(_.map(_._1)).sum
  }


  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val isValidIndex = isValidIndexF(input.length, input.head.length)
    val findValidNeighbours = findValidNeighboursF(isValidIndex)

    @tailrec
    def findLeft(c: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      if (c.last._2 > 0 && input(c.last._1)(c.last._2 - 1).isDigit) {
        findLeft(c :+ (c.last._1, c.last._2 - 1))
      }
      else {
        c
      }
    }

    @tailrec
    def findRight(c: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      if (c.last._2 < input.head.length - 1 && input(c.last._1)(c.last._2 + 1).isDigit) {
        findRight(c :+ (c.last._1, c.last._2 + 1))
      }
      else {
        c
      }
    }

    input.indices.flatMap(row => {
        input(row).indices.map(col => (row, col))
          .filter({ case (r, c) => input(r)(c) == '*' })
          .map({ case (r, c) =>
            val n = findValidNeighbours((r, c), true).filter({ case (r, c) => input(r)(c).isDigit }).toSet
            val currentSet = mutable.Set.from(n)
            val resultNumbers = mutable.ArrayBuffer.empty[Int]
            while (currentSet.nonEmpty) {
              val curr = currentSet.head
              currentSet.remove(curr)
              val left = findLeft(Seq(curr)).sorted
              val right = findRight(Seq(curr)).tail.sorted
              currentSet.filterInPlace(i => !(left ++ right).toSet.contains(i))
              val number = (left ++ right).map({ case (r, c) => input(r)(c) }).mkString.toInt
              resultNumbers += number
            }
            if (resultNumbers.size == 2) resultNumbers.product else 0
          })
      })
      .sum
    //          .flatMap(_.map(_._1)).sum
    //    0
  }
}