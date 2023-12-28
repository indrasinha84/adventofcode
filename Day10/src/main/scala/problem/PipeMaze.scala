package problem

import extensions.SeqExtensions.*
import model.graph.{Edge, Node, Path, Vertice}
import utils.isValidNodeF

import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object PipeMaze {

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.toSeq).toSeq
  }.get

  implicit def nodeOrdering[A <: Node]: Ordering[A] =
    Ordering.by(e => (e.row, e.col))


  def findValidNodesForStartF(input: Seq[Seq[Char]])(edge: Node) = {
    var neighbours = Seq.empty[Node]
    var nextNode = Node(edge.row + 1, edge.col)
    if (Seq('|', 'L', 'J').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row - 1, edge.col)
    if (Seq('|', '7', 'F').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row, edge.col + 1)
    if (Seq('-', '7', 'J').contains(input(nextNode.row)(nextNode.col)))
      neighbours = neighbours :+ nextNode
    nextNode = Node(edge.row, edge.col - 1)
    if (Seq('-', 'L', 'F').contains(input(nextNode.row)(nextNode.col)))
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
    val validNodes = input.toGraphNodes().filter(n => input(n.row)(n.col) != '.')
    val start = validNodes.find(n => input(n.row)(n.col) == 'S').get
    val isValidNode = isValidNodeF(input.length, input.head.length)
    val findValidNodesForStart = findValidNodesForStartF(input)

    val pathFinder = findPath(findValidNodesForStart, input, isValidNode)

    val vertices = validNodes.toSeq.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => Edge(1, n)).toSet
      Vertice(node = v, adjoiningNodes = edges)
    })
    val verticesMap = vertices.map(v => v.node -> v).toMap
    val unvisitedNodes: mutable.Set[Vertice] = mutable.Set.from(vertices)
    val visitedNodes = mutable.Set.empty[Vertice]
    val pathMap = mutable.SortedMap.from(validNodes.map(n => n -> (if (n == start) Path(null, 0) else Path(null, Int.MaxValue))))
    var changesDone = true
    while (changesDone) {
      changesDone = false
      val minValue = pathMap.map(_._2.distance).min
      pathMap.filter(_._2.distance == minValue).keys.foreach(currentNode => {
        verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedNodes.map(_.node).contains(e.endNode)).foreach(adj => {
          val adjPath = pathMap(adj.endNode)
          pathMap(adj.endNode) = adjPath.copy(distance = pathMap(currentNode).distance - adj.distance, previous = currentNode)
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
    0L
  }
}