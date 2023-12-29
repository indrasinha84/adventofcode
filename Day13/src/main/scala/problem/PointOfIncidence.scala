package problem

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object PointOfIncidence {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().mkString(",").split(",,").map(_.split(",").toSeq).toSeq
  }.get


  @tailrec
  private def getMirrorSize(base: Seq[String], up: Int, down: Int, result: Int, smudgeSwitchDone: Boolean): Int = {
    if (up >= 0 && down < base.length) {
      val comparisonCount = base(up).zip(base(down)).count(c => c._1 == c._2)
      if (comparisonCount == base(up).length) {
        getMirrorSize(base, up - 1, down + 1, result, smudgeSwitchDone)
      }
      else if (!smudgeSwitchDone && comparisonCount == base(up).length - 1) {
        getMirrorSize(base, up - 1, down + 1, result, true)
      }
      else {
        0
      }
    }
    else {
      if ((up == -1 || down == base.length) && smudgeSwitchDone) result + 1 else 0
    }
  }

  private def checkMirrorSize(patterns: Seq[String], smudgeSwitchDone: Boolean): Long = {
    val base = patterns
    patterns.zipWithIndex.sliding(2)
      .map({ case Seq((_, up), (_, down)) => getMirrorSize(base, up, down, up, smudgeSwitchDone) })
      .maxOption.getOrElse(0)
  }

  def problem1(filePath: String): Long = readFile(filePath).map(patterns =>
    checkMirrorSize(patterns, true) * 100L + checkMirrorSize(patterns.transpose.map(_.mkString), true)).sum

  def problem2(filePath: String): Long = readFile(filePath)
    .map(patterns => checkMirrorSize(patterns, false) * 100L + checkMirrorSize(patterns.transpose.map(_.mkString), false)).sum

}