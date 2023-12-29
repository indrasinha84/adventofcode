package problem

import extensions.SeqExtensions.*
import model.graph.{Edge, Node, Path, Vertice}
import utils.isValidNodeF

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.io.Source
import scala.math.abs
import scala.util.Using

object PipeMaze {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.toSeq).toSeq
  }.get

  implicit def nodeOrdering[A <: Node]: Ordering[A] =
    Ordering.by(e => (e.row, e.col))


  private def findValidNodesForStartF(isValidNode: Node => Boolean, input: Seq[Seq[Char]])(edge: Node) = {
    var neighbours = Seq.empty[Node]
    var nextNode = Node(edge.row + 1, edge.col)
    if (isValidNode(nextNode) && Seq('|', 'L', 'J').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row - 1, edge.col)
    if (isValidNode(nextNode) && Seq('|', '7', 'F').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row, edge.col + 1)
    if (isValidNode(nextNode) && Seq('-', '7', 'J').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row, edge.col - 1)
    if (isValidNode(nextNode) && Seq('-', 'L', 'F').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    neighbours
  }

  private def findPath(findValidNodesForStart: Node => Seq[Node], input: Seq[Seq[Char]], isValidNode: Node => Boolean)(edge: Node) = {
    (input(edge.row)(edge.col) match
      case '|' => Seq(Node(edge.row + 1, edge.col), Node(edge.row - 1, edge.col))
      case '-' => Seq(Node(edge.row, edge.col + 1), Node(edge.row, edge.col - 1))
      case 'L' => Seq(Node(edge.row - 1, edge.col), Node(edge.row, edge.col + 1))
      case 'J' => Seq(Node(edge.row, edge.col - 1), Node(edge.row - 1, edge.col))
      case '7' => Seq(Node(edge.row, edge.col - 1), Node(edge.row + 1, edge.col))
      case 'F' => Seq(Node(edge.row, edge.col + 1), Node(edge.row + 1, edge.col))
      case 'S' => findValidNodesForStart(edge)
      case _ => Nil
      ).filter(n => isValidNode(n) && !(input(n.row)(n.col) == '.'))
  }

  def problem1(filePath: String): Long = {
    val input = readFile(filePath)
    val validNodes = input.toGraphNodes.filter(n => input(n.row)(n.col) != '.')
    val start = validNodes.find(n => input(n.row)(n.col) == 'S').get
    val isValidNode = isValidNodeF(input.length, input.head.length)
    val findValidNodesForStart = findValidNodesForStartF(isValidNode, input)

    val pathFinder = findPath(findValidNodesForStart, input, isValidNode)

    val vertices = validNodes.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => Edge(1, n)).toSet
      Vertice(node = v, adjoiningNodes = edges)
    })
    val verticesMap = vertices.map(v => v.node -> v).toMap
    val unvisitedNodes: mutable.Set[Vertice] = mutable.Set.from(vertices)
    val visitedNodes = mutable.Set.empty[Vertice]
    val pathMap = mutable.SortedMap.from(validNodes.map(n => n -> (if (n == start) Path(Seq.empty, 0) else Path(Seq.empty, Int.MaxValue))))
    var changesDone = true
    while (changesDone) {
      changesDone = false
      val minValue = pathMap.map(_._2.distance).min
      pathMap.filter(_._2.distance == minValue).keys.foreach(currentNode => {
        verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedNodes.map(_.node).contains(e.endNode)).foreach(adj => {
          val adjPath = pathMap(adj.endNode)
          pathMap(adj.endNode) = adjPath.copy(distance = pathMap(currentNode).distance - adj.distance, previous = adjPath.previous :+ currentNode)
          changesDone = true
        })
        visitedNodes.add(verticesMap(currentNode))
        unvisitedNodes.remove(verticesMap(currentNode))
      })
    }
    pathMap.values.map(_.distance).min.abs
  }

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    val validNodes = input.toGraphNodes.filter(n => input(n.row)(n.col) != '.')
    val start = validNodes.find(n => input(n.row)(n.col) == 'S').get
    val isValidNode = isValidNodeF(input.length, input.head.length)
    val findValidNodesForStart = findValidNodesForStartF(isValidNode, input)

    val pathFinder = findPath(findValidNodesForStart, input, isValidNode)

    val vertices = validNodes.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => Edge(1, n)).toSet
      Vertice(node = v, adjoiningNodes = edges)
    })
    val verticesMap = vertices.map(v => v.node -> v).toMap
    val unvisitedNodes: mutable.Set[Vertice] = mutable.Set.from(vertices)
    val visitedNodes = mutable.Set.empty[Vertice]
    val pathMap = mutable.SortedMap.from(validNodes.map(n => n -> (if (n == start) Path(Seq.empty, 0) else Path(Seq.empty, Int.MaxValue))))
    var changesDone = true
    while (changesDone) {
      changesDone = false
      val minValue = pathMap.map(_._2.distance).min
      pathMap.filter(_._2.distance == minValue).keys.foreach(currentNode => {
        verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedNodes.map(_.node).contains(e.endNode)).foreach(adj => {
          val adjPath = pathMap(adj.endNode)
          pathMap(adj.endNode) = adjPath.copy(distance = pathMap(currentNode).distance - adj.distance, previous = adjPath.previous :+ currentNode)
          changesDone = true
        })
        visitedNodes.add(verticesMap(currentNode))
        unvisitedNodes.remove(verticesMap(currentNode))
      })
    }
    val lastPoint = pathMap.find(_._2.distance == pathMap.values.map(_.distance).min).get._1

    val indexedNodeMap = mutable.Map.empty[Int, Node]
    indexedNodeMap.update(0, lastPoint)
    // Reverse the left and right with max and min if you get negative results
    var leftPathNext = pathMap(lastPoint).previous.max
    var leftIndex = 1
    while (leftPathNext != start) {
      indexedNodeMap.update(leftIndex, leftPathNext)
      leftPathNext = pathMap(leftPathNext).previous.max
      leftIndex += 1
    }

    var rightPathNext = pathMap(lastPoint).previous.min
    var rightIndex = -1
    while (rightPathNext != start) {
      indexedNodeMap.update(rightIndex, rightPathNext)
      rightPathNext = pathMap(rightPathNext).previous.min
      rightIndex -= 1
    }
    indexedNodeMap.update(rightIndex, start)
    val minValue = pathMap.values.map(_.distance).min.abs
    val sortedNodes = indexedNodeMap.toSeq.sortBy(_._1).map(_._2)
    (Seq(sortedNodes.last, start) +: sortedNodes.sliding(2).toSeq).map({ case Seq(previous, next) =>
      (next.row, next.col) match
        case _ if next.row == previous.row && next.col == previous.col + 1 => 1 //R
        case _ if next.row == previous.row && next.col == previous.col - 1 => 0 //L
        case _ if next.row == previous.row + 1 && next.col == previous.col => next.col + 1 //D
        case _ if next.row == previous.row - 1 && next.col == previous.col => -next.col //U
    }).sum - (2 * minValue) + 1
  }
}