package problem

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object WaitForIt {
  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().toArray match
      case Array(time, distance) => (time.split(' ').filterNot(_.isBlank).tail.map(_.toLong), distance.split(' ').filterNot(_.isBlank).tail.map(_.toLong))
  }.get

  @tailrec
  private def findFirstSuccessfulPress(t: Long, d: Long, p: Long): Long = if ((p * (t - p)) > d) p else findFirstSuccessfulPress(t, d, p + 1)

  @tailrec
  private def findLastSuccessfulPress(t: Long, d: Long, p: Long): Long = if ((p * (t - p)) > d) p else findLastSuccessfulPress(t, d, p - 1)

  def problem1(filePath: String): Long = {
    val (time, distance) = readFile(filePath)
    val timeAndDistance = time.zip(distance)
    timeAndDistance.map({ case (t, d) =>
      val minimumPress = findFirstSuccessfulPress(t, d, 1)
      val lastPress = findLastSuccessfulPress(t, d, t - minimumPress)
      lastPress - minimumPress + 1
    }).product
  }

  def problem2(filePath: String): Long = {
    val (time, distance) = readFile(filePath)
    val t = time.map(_.toString).mkString.toLong
    val d = distance.map(_.toString).mkString.toLong
    val minimumPress = findFirstSuccessfulPress(t, d, 1)
    val lastPress = findLastSuccessfulPress(t, d, t - minimumPress)
    lastPress - minimumPress + 1
  }
}