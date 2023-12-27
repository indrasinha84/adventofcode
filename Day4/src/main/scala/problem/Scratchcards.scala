package problem

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Scratchcards {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    val splitChars = Array(' ', ':')
    file
      .getLines()
      .map(_.split('|') match {
        case Array(myNumbers, winningNumbers) =>
          val left = myNumbers.split(splitChars).filterNot(_.isBlank)
          (left(1).toInt, left.takeRight(left.length - 2).map(_.toInt).toSet, winningNumbers.split(splitChars).filterNot(_.isBlank).map(_.toInt).toSet)
      })
      .toArray
  }.get

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    input.map(r => {
      val numberOfWinnings = (r._2 intersect r._3).size
      if (numberOfWinnings == 0) 0 else Math.pow(2, numberOfWinnings - 1).toInt
    }).sum
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val winningCountMap = input.map(r => r._1 -> (r._2 intersect r._3).size).toMap
    val resultMap = mutable.Map.from(winningCountMap.view.mapValues(_ => 1))
    input.foreach(r => {
      Seq.range(r._1 + 1, r._1 +  winningCountMap(r._1) + 1).foreach(i => resultMap += (i -> (resultMap(i) + resultMap(r._1))))
    })
    resultMap.values.sum
  }
}