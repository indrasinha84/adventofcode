package problem

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.{break, breakable}
import scala.collection.parallel.CollectionConverters.*

object Snowverload {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file.getLines().map(_.split(':') match
      case Array(left, right) => (left, right.trim.split(' ').toSet)
    ).toSet.flatMap(s => s._2.map(r => Set(s._1, r)))
  }.get

  @tailrec
  def getChainedGroups(input: Set[Set[String]], chain: Seq[Set[String]]): Seq[Set[String]] = {
    //    println("getChainedGroups called")
    if (input.isEmpty) {
      chain
    }
    else {
      val nextElement = if (chain.isEmpty) input.headOption else input.find(p => (p intersect chain.last).nonEmpty)
      nextElement match
        case Some(value) =>
          val lastGroup = chain.lastOption.getOrElse(Set.empty) | value
          getChainedGroups(input - value, if (chain.size > 1) chain.take(chain.length - 1) :+ lastGroup else Seq(lastGroup))
        case None => getChainedGroups(input - input.head, chain :+ input.head)
    }
  }


  @tailrec
  private def findGroupForEveryStep(startSet: Set[String], allCombos: Map[String, Set[String]], result: Seq[Set[String]]): Seq[Set[String]] = {
    val nextSet = startSet.flatMap(allCombos) & allCombos.keySet
    val newCombos = allCombos.removedAll(startSet)
    //    println(nextSet.size)
    if (nextSet.nonEmpty)
      findGroupForEveryStep(nextSet & newCombos.keySet, allCombos.removedAll(startSet), result :+ nextSet)
    else {
      result
    }

  }

  def buildSparseMatrix(input: Set[Set[String]]) = {
    val allCombos = input.flatten.toSeq.sorted.zipWithIndex.map({ case (k, i) => k -> (i, input.filter(_.contains(k)).flatten - k)
    }).toMap
    allCombos.toSeq.sorted.foreach(println)
    val sparseMatrix = mutable.ArrayBuffer.fill(allCombos.size, allCombos.size)("")
    allCombos.foreach(b => {
      val i = b._2._1
      b._2._2.map(allCombos).map(_._1).foreach(j => {
        sparseMatrix(i)(j) = "*"
        sparseMatrix(j)(i) = "*"
      })
    })
    import extensions.StringExtensions.*
    sparseMatrix.map(_.map(_.lpad(' ', 1)).mkString(", ")).foreach(println)
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val allCombos = input.flatten.map(k => k -> (input.filter(_.contains(k)).flatten - k)).toMap
    val results = allCombos.keySet.map(s => s -> findGroupForEveryStep(Set(s), allCombos, Seq.empty)).toMap
    val combos = results("kkg").find(_.size == 17).get
    val combinationsToCheck =
      input.filter(p => {
          (p & combos).nonEmpty
        })
        .toSeq.combinations(3)
        .filter(_.flatten.toSet.size == 6)
        .toSeq
    println(s"Combinations found ${combinationsToCheck.size}")
    combinationsToCheck.par
      .map(s => getChainedGroups(input -- s.toSet, Seq.empty)).filter(_.size > 1).foreach(println)

    2810
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}