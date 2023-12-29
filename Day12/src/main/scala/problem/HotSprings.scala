package problem

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source
import scala.util.Using

object HotSprings {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split(' ') match
        case Array(str, template) => (template.split(',').map(_.toInt).toSeq, str, 1L)).toSeq
  }.get


  import extensions.StringExtensions.*

  @tailrec
  private def findValidCombinations(templateAndRemaining: Seq[(Seq[Int], String, Long)], cnt: Long): Long = {
    if (templateAndRemaining.isEmpty) {
      cnt
    }
    else {
      val newCnt = cnt + templateAndRemaining.filter({ case (template, remaining, _) => template.isEmpty && !remaining.contains('#') }).map(_._3).sum
      val newTR = templateAndRemaining
        .filter({ case (template, _, _) => template.nonEmpty })
        .groupMap({ case (template, remaining, _) => (template, remaining) })(_._3).view.mapValues(_.sum).map(x => (x._1._1, x._1._2, x._2)).toSeq
        .flatMap({ case (template, remaining, multiplier) =>
          val newRemaining = remaining.removeFirstDots()
          template match
            case t =>
              val desiredText = Seq.fill(t.head)('#').mkString
              val desiredTextWithDot = desiredText + '.'
              if ((newRemaining.length >= desiredTextWithDot.length && newRemaining.startsWith(desiredTextWithDot)) || newRemaining == desiredText) {
                Seq((template.tail, newRemaining.stripPrefix(desiredText).stripPrefix("."), multiplier))
              }
              else if (newRemaining.length >= desiredTextWithDot.length && (!newRemaining.substring(0, desiredText.length).contains('.') && (newRemaining.substring(0, desiredText.length).contains('?') || newRemaining.substring(0, desiredText.length).contains('#'))) &&
                (newRemaining.substring(desiredText.length, desiredTextWithDot.length).contains('.') || newRemaining.substring(desiredText.length, desiredTextWithDot.length).contains('?'))
              ) {
                if (newRemaining.head == '?') {
                  Seq( (template.tail, newRemaining.takeRight(newRemaining.length - desiredTextWithDot.length), multiplier),
                  (template, newRemaining.tail, multiplier))
                }
                else {
                  Seq((template.tail, newRemaining.takeRight(newRemaining.length - desiredTextWithDot.length), multiplier))
                }

              }

              else if (newRemaining.length == desiredText.length && (!newRemaining.contains('.') && newRemaining.contains('?'))) {
                Seq((template.tail, "", multiplier))
              }
              else if (newRemaining.headOption.contains('?')) {
                Seq((template, newRemaining.tail, multiplier))
              }
              else {
                Seq.empty
              }
        })
      findValidCombinations(newTR, newCnt)
    }


  }


  def problem1(filePath: String): Long = readFile(filePath).par.map(s => findValidCombinations(Seq(s), 0L)).sum

  def problem2(filePath: String): Long = readFile(filePath)
    .map({ case (template, str, multiplier) => (Seq.fill(5)(template).flatten, Seq.fill(5)(str).mkString("?"), multiplier) })
    .par.map(s => findValidCombinations(Seq(s), 0L)).sum

}