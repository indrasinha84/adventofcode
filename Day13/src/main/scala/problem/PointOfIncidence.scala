package problem

import scala.io.Source
import scala.util.Using

object PointOfIncidence {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().mkString(",").split(",,").map(_.split(",").toSeq).toSeq
  }.get


  def getMirrorSize(base: Seq[String], s: Int): Int = {
    var mirrorSize = 0
    var up = s
    var down = s + 1
    while (up >= 0 && down < base.length && base(up) == base(down)) {
      mirrorSize += 1
      up -= 1
      down += 1
    }
    if (up == -1 || down == base.length) s + 1 else 0
  }

  private def checkMirrorSize(patterns: Seq[String]): Long = {
    val base = patterns
    patterns.zipWithIndex.sliding(2)
      .filter({ case Seq((left, _), (right, _)) => left.mkString == right.mkString })
      .map({ case Seq((_, left), (_, _)) => left })
      .map(start => {
        getMirrorSize(base, start)
      }).maxOption.getOrElse(0)
  }

  def problem1(filePath: String): Long = readFile(filePath).map(patterns => checkMirrorSize(patterns) * 100L + checkMirrorSize(patterns.transpose.map(_.mkString))).sum

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    0L
  }
}