package problem

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object WaitForIt {
  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().toArray match
      case Array(time, distance) => (time.split(' ').filterNot(_.isBlank).tail.map(_.toInt), distance.split(' ').filterNot(_.isBlank).tail.map(_.toInt))
  }.get

  @tailrec
  private def findFirstSuccessfulPress(t: Int, d: Int, p: Int): Int = if ((p * (t - p)) > d) p else findFirstSuccessfulPress(t, d, p + 1)

  @tailrec
  private def findLastSuccessfulPress(t: Int, d: Int, p: Int): Int = if ((p * (t - p)) > d) p else findLastSuccessfulPress(t, d, p - 1)

  def problem1(filePath: String): Int = {
    val (time, distance) = readFile(filePath)
    val timeAndDistance = time.zip(distance)
    timeAndDistance.map({ case (t, d) =>
      val minimumPress = findFirstSuccessfulPress(t, d, 1)
      val lastPress = findLastSuccessfulPress(t, d, t - minimumPress)
      lastPress - minimumPress + 1
    }).product
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}