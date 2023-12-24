package problem

import scala.io.Source
import scala.util.Using
import utils.{findNeighbours, findValidNeighboursF, isValidIndexF}

import scala.collection.mutable

final case class Node(row: Int, col: Int)

final case class Path(previous: Set[Node], distance: Int)

object ALongWalk {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.toCharArray).toArray
  }.get


  private def findPath(input: Array[Array[Char]], isValidIndex: ((Int, Int)) => Boolean)(edge: Node) = {
    Seq(
      (if (isValidIndex(edge.row, edge.col - 1) && Seq('.', '>').contains(input(edge.row)(edge.col - 1))) Seq(Node(edge.row, edge.col - 1)) else Nil) ++
        (if (isValidIndex(edge.row, edge.col + 1) && Seq('.', '<').contains(input(edge.row)(edge.col + 1))) Seq(Node(edge.row, edge.col + 1)) else Nil) ++
        (if (isValidIndex(edge.row - 1, edge.col) && Seq('.', 'v').contains(input(edge.row - 1)(edge.col))) Seq(Node(edge.row - 1, edge.col)) else Nil) ++
        (if (isValidIndex(edge.row + 1, edge.col) && Seq('.', '^').contains(input(edge.row + 1)(edge.col))) Seq(Node(edge.row + 1, edge.col)) else Nil)
    ).flatten
  }


  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val start = Node(0, input.head.indexOf('.'))
    val end = Node(input.length - 1, input.last.lastIndexOf('.'))
    val isValidIndex = isValidIndexF(input.length, input.head.length)
    val pathFinder = findPath(input, isValidIndex)
    var changesDone = true
    val validNodes = input.indices.flatMap(row => input(row).indices.map(col => Node(row, col)))
      .filter(n => input(n.row)(n.col) != '#')

    implicit def orderingByName[A <: Node]: Ordering[A] =
      Ordering.by(e => (e.row, e.col))

    val pathMap = mutable.SortedMap.from(validNodes.map(n => n -> (if (n == start) Path(Set.empty, 0) else Path(Set.empty, Int.MaxValue))))
    while (changesDone) {
      changesDone = false
      pathMap.filter(_._1 != start).foreach(p => {
        val immediateNeighbours = pathFinder(p._1)
          .filter(n => pathMap(n).distance != Int.MaxValue)
          .filter(n => n == start || (pathMap(n).previous - p._1).nonEmpty)
        immediateNeighbours.foreach(n => {
          val curr = pathMap(p._1)
          val prev = pathMap(n)
          if (curr.distance == Int.MaxValue || curr.distance > prev.distance - 1) {
            pathMap(p._1) = Path(previous = Set(n), distance = prev.distance - 1)
            changesDone = true
          }
          else if (curr.distance == prev.distance - 1 && !curr.previous.contains(n)) {
            changesDone = true
            pathMap(p._1) = Path(previous = curr.previous + n, distance = prev.distance - 1)
          }

        })
      })
    }
    pathMap(end).distance.abs
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}