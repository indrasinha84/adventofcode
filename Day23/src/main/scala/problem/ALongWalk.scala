package problem

import scala.io.Source
import scala.util.Using
import utils.{findNeighbours, findValidNeighboursF, isValidIndexF}

import scala.annotation.tailrec
import scala.collection.mutable

final case class Node(row: Int, col: Int)

final case class Edge(distance: Int, endNode: Node)

final case class Vertice(node: Node, adjoiningNodes: Set[Edge])

final case class Path(previous: Node, distance: Int)

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

  implicit def orderingByName[A <: Node]: Ordering[A] =
    Ordering.by(e => (e.row, e.col))

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val start = Node(0, input.head.indexOf('.'))
    val end = Node(input.length - 1, input.last.lastIndexOf('.'))
    val isValidIndex = isValidIndexF(input.length, input.head.length)
    val pathFinder = findPath(input, isValidIndex)
    var changesDone = true
    val validNodes = input.indices.flatMap(row => input(row).indices.map(col => Node(row, col)))
      .filter(n => input(n.row)(n.col) != '#')
    val pathMap = mutable.SortedMap.from(validNodes.map(n => n -> (if (n == start) Path(null, 0) else Path(null, Int.MaxValue))))
    while (changesDone) {
      changesDone = false
      pathMap.filter(_._1 != start).foreach(p => {
        val immediateNeighbours = pathFinder(p._1)
          .filter(n => pathMap(n).distance != Int.MaxValue)
          .filter(n => n == start || (pathMap(n).previous != p._1))
        immediateNeighbours.foreach(n => {
          val curr = pathMap(p._1)
          val prev = pathMap(n)
          if (curr.distance == Int.MaxValue || curr.distance > prev.distance - 1) {
            pathMap(p._1) = Path(previous = n, distance = prev.distance - 1)
            changesDone = true
          }
          else if (curr.distance == prev.distance - 1 && curr.previous != n) {
            changesDone = true
            pathMap(p._1) = Path(previous = n, distance = prev.distance - 1)
          }

        })
      })
    }
    pathMap(end).distance.abs
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath).map(_.map(_.toString.replace('>', '.').replace('<', '.')
      .replace('^', '.').replace('v', '.').head))
    val start = Node(0, input.head.indexOf('.'))
    val end = Node(input.length - 1, input.last.lastIndexOf('.'))
    val isValidIndex = isValidIndexF(input.length, input.head.length)
    val pathFinder = findPath(input, isValidIndex)
    val validNodes = input.indices.flatMap(row => input(row).indices.map(col => Node(row, col)))
      .filter(n => input(n.row)(n.col) != '#')
    val multiNodes = Set(start, end) ++ validNodes.filter(n => pathFinder(n).size > 2).toSet

    @tailrec
    def addTillNextVertice(curr: Node, last: Node, edge: Edge): Edge = {
      val nextNodes = pathFinder(curr).toSet - last
      if ((nextNodes intersect multiNodes).nonEmpty) {
        edge.copy(distance = edge.distance + 1, endNode = nextNodes.head)
      }
      else {
        addTillNextVertice(nextNodes.head, curr, edge.copy(distance = edge.distance + 1, endNode = nextNodes.head))
      }

    }

    val vertices = multiNodes.toSeq.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => addTillNextVertice(n, v, Edge(1, n))).toSet
      Vertice(node = v, adjoiningNodes = edges)
    })
    val verticesMap = vertices.map(v => v.node -> v).toMap
    val unvisitedNodes: Set[Vertice] = vertices.toSet

    def findLongestPath(distance: Int, current: Vertice, unvisited: Set[Vertice], cnt: Int): Int = {
      if (current == verticesMap(start)) {
        if (distance < cnt) cnt else distance
      }
      else {
        current.adjoiningNodes.filter(a => unvisited.map(_.node).contains(a.endNode)).map(adj => {
          findLongestPath(distance, verticesMap(adj.endNode), unvisited - current, cnt + adj.distance)
        }).maxOption.getOrElse(0)
      }
    }

    findLongestPath(0, verticesMap(end), unvisitedNodes, 0)
  }
}