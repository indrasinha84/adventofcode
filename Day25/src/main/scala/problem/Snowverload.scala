package problem

import extensions.StringExtensions.*

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Random, Using}

object Snowverload {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file.getLines().map(_.split(':') match
      case Array(left, right) => (left, right.trim.split(' ').toSeq)
    ).toSeq.flatMap(s => s._2.map(r => Seq(s._1, r)))
  }.get

  @tailrec
  def getChainedGroups(input: Set[Set[String]], chain: Seq[Set[String]]): Seq[Set[String]] = {
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
    if (nextSet.nonEmpty)
      findGroupForEveryStep(nextSet & newCombos.keySet, allCombos.removedAll(startSet), result :+ nextSet)
    else {
      result
    }

  }

  final case class Graph(adj: Seq[Seq[Int]], verticesList: Seq[Set[String]])

  private def buildGraph(input: Set[Set[String]]) = {
    val allCombos = input.flatten.toSeq.sorted.zipWithIndex.map({ case (k, i) => k -> (i, input.filter(_.contains(k)).flatten - k) }).sortBy(_._2._1)
    val verticesList = Seq.from(allCombos.map({ case (k, (_, _)) => Set(k) }))
    val adjacencyMatrix = allCombos.map(b => allCombos.map(c => if (b._2._2.contains(c._1)) 1 else 0))
    Graph(adjacencyMatrix, verticesList)
  }

  private def contractMatrix(graph: Graph, from: Int, to: Int) = {
    val newAdj = graph.adj.zipWithIndex.map({ case (row, i) =>
      row.zipWithIndex.map({ case (item, j) =>
        if (i == to) {
          0
        }
        else if (j == to) {
          0
        }
        else if (i == from && i != j) {
          item + graph.adj(to)(j)
        }
        else if (j == from && i != j) {
          item + graph.adj(i)(to)
        }
        else {
          item
        }
      })
    })
    val newVerticesList = graph.verticesList.zipWithIndex.map({ case (s, i) => if (i == from) s ++ graph.verticesList(to) else if (i == to) Set.empty else s })
    graph.copy(adj = newAdj, verticesList = newVerticesList)
  }

  @tailrec
  private def bisectGraph(graph: Graph): Graph = {
    val rand = Random

    @tailrec
    def generateFrom: (Int, Seq[Int]) = {
      val nonZeroFroms = graph.verticesList.zipWithIndex.filter(i => graph.verticesList(i._2).nonEmpty).map(_._2)
      val from = rand.nextInt(graph.adj.length - 1)
      val nonZeroTos = graph.adj.zipWithIndex.filter(i => i._2 != from && graph.adj(i._2)(from) > 0).map(_._2)
      if (nonZeroFroms.contains(from) && nonZeroTos.nonEmpty) (from, nonZeroTos) else generateFrom
    }

    val (from, nonZeroTos) = generateFrom

    @tailrec
    def generateTo: Int = {
      val nextNum = if (nonZeroTos.length > 1) rand.nextInt(nonZeroTos.length - 1) else 0
//      if (nonZeroTos.isEmpty)
//        println
      val num = nonZeroTos(nextNum)
      if (nonZeroTos.contains(num)) num else generateTo
    }

    val to = generateTo
    val newGraph = contractMatrix(graph, from, to)
    //    println(s"After contracting from $from to $to")
    //    newGraph.adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)
    if (newGraph.verticesList.count(_.nonEmpty) > 2) bisectGraph(newGraph) else newGraph
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val inputSet = input.map(_.toSet).toSet
    val graph = buildGraph(inputSet)
    var bisectedGraph = buildGraph(inputSet)
//    bisectedGraph.adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)
    //    bisectedGraph = bisectGraph(graph)
//    println
    //    bisectedGraph.adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)
    while (bisectedGraph.adj.flatten.exists(_ != 3)) {
      bisectedGraph = bisectGraph(graph)
//      bisectedGraph.adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)
      println
    }
//    println
//    graph.verticesList.foreach(println)
    graph.verticesList.map(_.size).sum
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}