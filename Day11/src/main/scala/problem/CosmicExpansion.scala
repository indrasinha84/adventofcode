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

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}