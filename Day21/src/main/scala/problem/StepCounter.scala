package problem


import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object StepCounter {


  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.toCharArray).toArray
  }.get


  private def findPossiblePlotsUsingCacheF(takeNextStep: ((Int, Int), Int) => Set[(Int, Int)], getBoundedIndex: ((Int, Int)) => (Int, Int)) = (p: (Int, Int)) => {
    val boundedIndex = getBoundedIndex(p)
    val offsets = (p._1 - boundedIndex._1, p._2 - boundedIndex._2)
    cache.getOrElseUpdate(p, takeNextStep(boundedIndex, 1)).map(p => (p._1 + offsets._1, p._2 + offsets._2))
  }

  private def getBoundedIndexF(rowLength: Int, colLength: Int)(p: (Int, Int)) = {
    (if (p._1.sign == -1) rowLength - 1 + ((p._1 + 1) % rowLength) else p._1 % rowLength,
      if (p._2.sign == -1) colLength - 1 + ((p._2 + 1) % colLength) else p._2 % colLength)
  }

  private def isGardenPlotF(input: Array[Array[Char]]) = {
    val rowLength = input.length
    val colLength = input.head.length
    val getBoundedIndex = getBoundedIndexF(rowLength, colLength)
    (p: (Int, Int)) => {
      val (boundedRow, boundedCol) = getBoundedIndex(p)
      Seq('.', 'S').contains(input(boundedRow)(boundedCol))
    }
  }


  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val start = input.indices.flatMap(i => input(i).indices.map(j => (i, j))).find { case (r, c) => input(r)(c) == 'S' }.get
    var next = mutable.Set(start)
    val isGardenPlot = isGardenPlotF(input)
    Range.inclusive(1, 100).foreach(_ => {
      val nextSet = next.toSet
      next = mutable.Set.empty[(Int, Int)]
      nextSet.foreach(i => {
        val (r, c) = i
        Seq((r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)).filter(isGardenPlot)
          .foreach(p => next.+=(p))
      })
    })
    next.toSet.size

  }

  def takeNextStepF(isGardenPlot: ((Int, Int)) => Boolean)(start: (Int, Int), steps: Int) = {
    var next = mutable.Set(start)
    Range.inclusive(1, steps).foreach(_ => {
      val nextSet = next.toSet
      next = mutable.Set.empty[(Int, Int)]
      nextSet.foreach(i => {
        val (r, c) = i
        Seq((r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)).filter(isGardenPlot)
          .foreach(p => next.+=(p))
      })
    })
    next.toSet
  }
  private val cache = mutable.Map.empty[(Int, Int), Set[(Int, Int)]]


  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val start = input.indices.flatMap(i => input(i).indices.map(j => (i, j))).find { case (r, c) => input(r)(c) == 'S' }.get
    var next = mutable.Set(start)
    val isGardenPlot = isGardenPlotF(input)
//    val findPossiblePlots = findPossiblePlotsF(isGardenPlot)
    //    val cache = input.indices.flatMap(i => input(i).indices.map(j => (i, j))).map(p => p -> findPossiblePlots(p)).toMap

    val getBoundedIndex = getBoundedIndexF(input.length, input.head.length)
    val takeNextStep = takeNextStepF(isGardenPlot)
    val findPossiblePlotsUsingCache = findPossiblePlotsUsingCacheF(takeNextStep, getBoundedIndex)
    //    Range.inclusive(1, 100).foreach(_ => {
    //      val nextSet = next.toSet
    //      next = mutable.Set.empty[(Int, Int)]
    //      nextSet.foreach(p => findPossiblePlotsUsingCache(p).foreach(p => next.+=(p)))
    //    })
//    val cache2 = input.indices.flatMap(i => input(i).indices.map(j => (i, j))).map(p => p -> takeNextStep(p, 10)).toMap
//    val findPossiblePlotsUsingCache2 = findPossiblePlotsUsingCacheF(getBoundedIndex, cache3)
    next = mutable.Set(start)
    Range.inclusive(1, 1000).foreach(_ => {
      val nextSet = next.toSet
      next = mutable.Set.empty[(Int, Int)]
      nextSet.foreach(p => findPossiblePlotsUsingCache(p).foreach(p => next.+=(p)))
    })
    next.toSet.size
  }
}