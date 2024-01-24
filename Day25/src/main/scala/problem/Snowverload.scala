package problem

import extensions.StringExtensions.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Random, Using}

final case class Edge(distance: Int, endNode: String)

final case class Vertice(name: String, adjoiningNodes: Set[String])

object Snowverload {
  //
  //  extension (listOfList: Seq[_]) {
  //    def toGraphNodes: Seq[String] = {
  //      listOfList.indices.map(col => Node(row, col))
  //    }
  //  }

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file.getLines().map(_.split(':') match
      case Array(left, right) => (left, right.trim.split(' ').toSeq)
    ).toSeq.flatMap(s => s._2.map(r => Seq(s._1, r)))
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

  private def buildAdjacencyMatrix(input: Set[Set[String]]) = {
    val allCombos = input.flatten.toSeq.sorted.zipWithIndex.map({ case (k, i) => k -> (i, input.filter(_.contains(k)).flatten - k) }).toMap
    val adjacencyMatrix = mutable.ArrayBuffer.fill(allCombos.size, allCombos.size)(0)
    allCombos.foreach(b => {
      val i = b._2._1
      b._2._2.map(allCombos).map(_._1).foreach(j => {
        adjacencyMatrix(i)(j) = 1
      })
    })
    adjacencyMatrix
  }

  private def contractMatrix(adj: ArrayBuffer[ArrayBuffer[Int]], from: Int, to: Int) = {
    val adjCopy = adj.map(_.toArray).toArray
    adjCopy(to).indices.foreach(j => {
      if (j != from) {
        adj(from)(j) += adj(to)(j)
        adj(j)(from) += adj(j)(to)
      }
    })
    adjCopy.indices.foreach(j => {
      val newRow = adj(j)
      newRow.remove(to)
      adj(j) = newRow
    })
    adj.remove(to)
    adj
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val inputSet = input.map(_.toSet).toSet
    var adj = buildAdjacencyMatrix(inputSet)

    while (adj.length > 2) {
      adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)
      println
      val rand = Random
      val from = rand.nextInt(adj.length - 1)
      val nonZeros = adj(from).zipWithIndex.filter(_._1 > 0).map(_._2)

      @tailrec
      def generateTo: Int = {
        val nextNum = if (nonZeros.length > 1) rand.nextInt(nonZeros.length - 1) else 0
        val num = nonZeros(nextNum)
        if (num != from) num else generateTo
      }

      val to = generateTo
      adj = contractMatrix(adj, from, to)
    }
    adj.map(_.map(_.toString.lpad(' ', 1)).mkString(", ")).foreach(println)

    val verticesMap = inputSet.flatten.toSeq.sorted.zipWithIndex.map({ case (k, i) => k -> (i, inputSet.filter(_.contains(k)).flatten - k) })
    val vertices = verticesMap.sortBy(_._2._1).map(v => {
      Vertice(name = v._1, adjoiningNodes = v._2._2)
    })
    2810
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}