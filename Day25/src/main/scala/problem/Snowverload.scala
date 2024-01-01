package problem

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.{break, breakable}

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


  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val allCombos = input.flatten.map(k => k -> input.filter(_.contains(k)).flatten).toMap

    //    allCombos.view.mapValues(_.size).toSeq.sortBy(_._2).reverse.foreach(println)

    def findChosenPool(applicableInput: Set[Set[String]]) = {
      var allMin = Int.MaxValue
      var allMinCurrent = ""
      var shortestSet = Set.empty[String]
      val allCombosForSetPool = applicableInput.flatten.map(k => k -> applicableInput.filter(_.contains(k)).flatten).toMap
      allCombosForSetPool.foreach(ac => {
        var traversalResults = Seq(Set(ac._1))
        var remainingCombos = allCombosForSetPool
        while (remainingCombos.nonEmpty) {
          val current = traversalResults.last
          traversalResults = traversalResults :+ traversalResults.last.flatMap(r => remainingCombos.getOrElse(r, Set.empty).filter(_ != r))
          remainingCombos = remainingCombos.filterNot(c => current.contains(c._1))
        }
        val highestValueIndex = traversalResults.zipWithIndex.maxBy(_._1.size)._2
        val rightPart = traversalResults.takeRight(traversalResults.size - highestValueIndex - 1)
        val secondHighestValue = rightPart.zipWithIndex.maxByOption(_._1.size).map(_._2).getOrElse(0)
        val minValue = traversalResults.slice(highestValueIndex, highestValueIndex + secondHighestValue + 1).map(_.size).min
        if (minValue < allMin) {
          allMin = minValue
          shortestSet = traversalResults.find(_.size == minValue).getOrElse(shortestSet)
          allMinCurrent = ac._1
        }
        //      println(traversalResults.slice(highestValueIndex, highestValueIndex + secondHighestValue + 1).map(_.size).min)
      })
      (shortestSet + allMinCurrent /* ++ shortestSet.flatMap(allCombos)*/).flatMap(s => applicableInput.filter(_.contains(s)))
    }

    //    println(shortestSet)
    //    println(allMinCurrent)
    import scala.collection.parallel.CollectionConverters._

//
//    //    val fixedThreadedPool: ExecutorService = Executors.newFixedThreadPool(16)
//    //    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(fixedThreadedPool)
//    val chosenPool = findChosenPool(input)
//    println(s"Chosen Pool 1 size is $chosenPool")
//    var remaining = chosenPool.size
//    val chosenPool2:  (Set[String], Set[Set[String]]) = chosenPool.par.map(s1 => {
//      remaining += -1
//      println(s"Finding chosen pool 2 for $s1 $remaining")
//      (s1, findChosenPool(input - s1))
//    }).seq.minBy(_._2.size)
//    remaining = chosenPool2._2.size
//    println(s"Chosen Pool 2 size is ${chosenPool2._2.size}")
//    val chosenPool3: (Set[String], Set[Set[String]], Set[String]) = chosenPool2._2.par.map(s2 => {
//      remaining += -1
//      println(s"Finding chosen pool 3 for $s2  $remaining")
//      (s2, findChosenPool(input - s2), chosenPool2._1)
//    }).seq.minBy(_._2.size)
//    val chosenPool4: (Set[String], Set[Set[String]], Set[String], Set[String]) = chosenPool3._2.par.map(s3 => {
//      remaining += -1
//      println(s"Finding chosen pool 4 for $s3  $remaining")
//      (s3, findChosenPool(input - s3), chosenPool3._1, chosenPool2._1)
//    }).seq.minBy(_._2.size)
//    print(s"Final Chosen pool is $chosenPool4")
    var result = 0
    //    chainedInputs.foreach(println)
    //    var i = 0

    def findCombo(cnt: Int, setsToRemove: Set[Set[String]], remainingSet: Set[Set[String]]): Unit = {
      //      println("findCombo called")
      if (cnt == 0) {
        if (result == 0) {
          //          i += 1
          //          println(i)
          val chainedGroups = getChainedGroups(input -- setsToRemove, Seq.empty)
          if (chainedGroups.size == 2) {
            result = chainedGroups.map(_.size).product
            println(s"Result is $result")
          }
        }

      }
      else {
        remainingSet.foreach(s => {
          findCombo(cnt - 1, setsToRemove + s, remainingSet - s)
        })
      }
    }

    input.foreach(s => {
      val groups = getChainedGroups(input -- Set(Set("jxm", "qns"), Set("plt", "mgb"), s), Seq.empty)
      println(s"For $s size is ${groups.size}")
    })


//    findCombo(3, Set.empty, chosenPool)
    result

  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}