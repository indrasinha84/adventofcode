package problem

import scala.collection.immutable.SeqMap
import scala.io.Source
import scala.util.Using

object LensLibrary {

  private final case class Box(lenses: SeqMap[String, Int] = SeqMap.empty)


  private def getHash: String => Int = _.toCharArray.map(_.toInt).foldLeft(0)((sum, elem) => ((sum + elem) * 17) % 256)

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file.getLines().mkString.split(",")
  }.get

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    input.map(getHash).sum

  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    input.foldLeft(Array.fill(256)(Box()))((sum, elem) => {
      val label = elem.split(Array('-', '=')).head
      val boxNumber = getHash(label)
      elem.replace(label, "").substring(0, 1) match
        case "-" =>  sum.updated(boxNumber, Box(SeqMap.from(sum(boxNumber).lenses - label)))
        case "=" =>  sum.updated(boxNumber, Box(sum(boxNumber).lenses + (label -> elem.takeRight(1).toInt)))
        case _ => sum
    }).zipWithIndex.flatMap(b => {
      b._1.lenses.zipWithIndex.map(l => (b._2 + 1) * (l._2 + 1) * l._1._2)
    }).sum
  }
}