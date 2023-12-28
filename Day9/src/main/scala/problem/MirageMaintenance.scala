package problem

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object MirageMaintenance {


  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split(' ').map(_.toLong).toSeq).toSeq
  }.get


  @tailrec
  private def findNextNumber(series: Seq[Long], nextNumber: Long): Long =
    if (!series.exists(_ != 0)) nextNumber else findNextNumber(series.sliding(2).map(s => s.last - s.head).toSeq, nextNumber + series.last)


  def problem1(filePath: String): Long = {
    val input = readFile(filePath)
    input.map(s => findNextNumber(s, 0L)).sum
  }

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    input.map(s => findNextNumber(s.reverse, 0L)).sum
  }
}