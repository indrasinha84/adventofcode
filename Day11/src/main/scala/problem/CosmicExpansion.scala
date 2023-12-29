package problem

import model.graph.Node

import scala.io.Source
import scala.util.Using

object CosmicExpansion {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.toSeq).toSeq
  }.get


  def expandGalaxy(input: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    input.flatMap(r => if (r.contains('#')) Seq(r) else Seq(r, r)).transpose.flatMap(r => if (r.contains('#')) Seq(r) else Seq(r, r)).transpose
  }

  def findEmptyRows(input: Seq[Seq[Char]]): Set[Long] = {
    input.zipWithIndex.filterNot(_._1.contains('#')).map(_._2.toLong).toSet
  }


  def findEmptyCols(input: Seq[Seq[Char]]): Set[Long] = {
    input.transpose.zipWithIndex.filterNot(_._1.contains('#')).map(_._2.toLong).toSet
  }

  private def numberTheGalaxies(input: Seq[Seq[Char]]): Seq[(Int, Node)] = {
    import extensions.SeqExtensions.*
    input.toGraphNodes.foldLeft((Seq.empty[(Int, Node)], 0))((sum, elem) => {
      if (input(elem.row)(elem.col) == '#') (sum._1 :+ (sum._2 + 1, elem), sum._2 + 1) else sum
    })._1
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val expandedGalaxies = expandGalaxy(input)
    val numberedGalaxies = numberTheGalaxies(expandedGalaxies)
    numberedGalaxies.combinations(2)
      .map({
        case Seq((_, source), (_, destination)) => (source.col - destination.col).abs + (source.row - destination.row).abs
      }).sum
  }

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    val numberedGalaxies = numberTheGalaxies(input)
    val emptyRows = findEmptyRows(input)
    val emptyCols = findEmptyCols(input)
    val multiplier = 1000000L
    numberedGalaxies.combinations(2)
      .map({
        case Seq((_, source), (_, destination)) =>
          val emptyRowsForThisPair = emptyRows.count(r => r < Math.max(source.row, destination.row) && r > Math.min(source.row, destination.row)) * (multiplier - 1)
          val emptyColsForThisPair = emptyCols.count(r => r < Math.max(source.col, destination.col) && r > Math.min(source.col, destination.col)) *  (multiplier - 1)
          (source.col - destination.col).abs + (source.row - destination.row).abs + emptyRowsForThisPair + emptyColsForThisPair
      }).sum
  }
}