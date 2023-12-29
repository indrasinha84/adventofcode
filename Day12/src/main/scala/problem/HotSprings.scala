package problem

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.parallel.CollectionConverters.*

object HotSprings {

  @tailrec
  private def findCombinations(strList: Seq[String]): Seq[String] = {
    if (strList.exists(!_.contains("?"))) {
      strList
    }
    else {
      findCombinations(strList.flatMap(s => Seq(s.replaceFirst("\\?", "."), s.replaceFirst("\\?", "#"))))
    }

  }

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split(' ') match
        case Array(str, template) => (template.split(',').map(_.toInt).toSeq, str)).toSeq
  }.get


  import extensions.StringExtensions.*

  @tailrec
  private def findValidCombinations(templateAndRemaining: Seq[(Seq[Int], String, String)], cnt: Int): Int = {
    if (templateAndRemaining.isEmpty) {
      cnt
    }
    else {
      val newCnt = cnt + templateAndRemaining.count({ case (template, remaining, selected) => template.isEmpty && !remaining.contains('#') && selected.nonEmpty })
      val newTR = templateAndRemaining
        .filter({ case (template, _, _) => template.nonEmpty })
        .map(tr => tr.copy(_2 = tr._2.removeFirstDots())).flatMap({ case (template, remaining, selected) =>
          template match
            case t =>
              val desiredText = Seq.fill(t.head)('#').mkString
              val desiredTextWithDot = desiredText + '.'
              //              println(s"Checking for $remaining and desiredText is $desiredTextWithDot and already selected is $selected")
              if ((remaining.length >= desiredTextWithDot.length && remaining.startsWith(desiredTextWithDot)) || remaining == desiredText) {
                Seq((template.tail, remaining.stripPrefix(desiredText).stripPrefix("."), selected + desiredTextWithDot))
              }
              else if (remaining.length >= desiredTextWithDot.length && (!remaining.substring(0, desiredText.length).contains('.') && (remaining.substring(0, desiredText.length).contains('?') || remaining.substring(0, desiredText.length).contains('#'))) &&
                (remaining.substring(desiredText.length, desiredTextWithDot.length).contains('.') || remaining.substring(desiredText.length, desiredTextWithDot.length).contains('?'))
              ) {
                if (remaining.head == '?') {
                  Seq((template.tail, remaining.takeRight(remaining.length - desiredTextWithDot.length), selected + desiredTextWithDot),
                    (template, remaining.tail, selected + '.'))
                }
                else {
                  Seq((template.tail, remaining.takeRight(remaining.length - desiredTextWithDot.length), selected + desiredTextWithDot))
                }

              }

              else if (remaining.length == desiredText.length && (!remaining.contains('.') && remaining.contains('?'))) {
                Seq((template.tail, "", selected + desiredText))
              }
              else if (remaining.headOption.contains('?')) {
                Seq((template, remaining.tail, selected + '.'))
              }
              else {
                Seq.empty
              }
        })
      findValidCombinations(newTR, newCnt)
    }


  }


  def problem1(filePath: String): Int = {
    val input = readFile(filePath).map(c => (c._1, c._2, ""))
    //    println(findValidCombinations(Seq(input(2)), 0))
    input.map(s => {
      val xxx = findValidCombinations(Seq(s), 0)
      //                    println(s"Combinations for ${s._2} is $xxx")
      xxx
    }).sum
    //    0
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val unfoldedInput = input.map({ case (template, str) => (Seq.fill(5)(template).flatten, Seq.fill(5)(str).mkString("?")) }).map(c => (c._1, c._2, ""))
    val i = AtomicInteger(0)

    unfoldedInput.par.map(s => {
      val in = i.getAndIncrement()
      println(s"$in Doing for ${s._2}")
      val xxx = findValidCombinations(Seq(s), 0)
      println(s"Completed for $in")
      xxx
    }).sum
  }
}