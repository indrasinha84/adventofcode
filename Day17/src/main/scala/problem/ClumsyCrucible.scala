package problem

import extensions.SeqExtensions.*
import model.graph.Node

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.io.Source
import scala.util.Using


final case class ClumsyNode(row: Int, col: Int, direction: String, steps: Int)

final case class ClumsyPath(previous: ClumsyNode, distance: Int)

final case class ClumsyEdge(distance: Int, endNode: ClumsyNode)

final case class ClumsyVertice(node: ClumsyNode, adjoiningNodes: Set[ClumsyEdge])


final case class Block(row: Int, col: Int, direction: String, allowedMoves: Seq[String], remainingMoves: Int)

object ClumsyCrucible {
  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.map(_.toString.toInt).toSeq).toSeq
  }.get

  implicit def nodeOrdering[A <: ClumsyNode]: Ordering[A] =
    Ordering.by(e => (e.row, e.col))

  def isValidNodeF(rowLength: Int, colLength: Int)(p: ClumsyNode) = p.row >= 0 && p.row < rowLength && p.col >= 0 && p.col < colLength && p.steps < 3


  def findPath(isValidNode: ClumsyNode => Boolean)(position: ClumsyNode): Seq[ClumsyNode] = {
    (position.row, position.col) match
      case (r, c) =>
        Seq(ClumsyNode(r, c + 1, "Right", if (position.direction == "Right") position.steps + 1 else 0),
          ClumsyNode(r, c - 1, "Left", if (position.direction == "Left") position.steps + 1 else 0),
          ClumsyNode(r + 1, c, "Down", if (position.direction == "Down") position.steps + 1 else 0),
          ClumsyNode(r - 1, c, "Up", if (position.direction == "Up") position.steps + 1 else 0)
        ).filter(isValidNode)
  }

  private def nodeToClumsyNode(n: Node) = {
    Seq.range(0, 3).flatMap(i => Seq("Right", "Left", "Up", "Down").map(d => ClumsyNode(n.row, n.col, d, i)))

  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val isValidNode = isValidNodeF(input.length, input.head.length)
    val validNodes = input.toGraphNodes.flatMap(nodeToClumsyNode).filter(isValidNode).filter(n => input(n.row)(n.col) != '.')
    val start = (0, 0)
    val end = (input.length - 1, input.head.length - 1)
    val pathFinder = findPath(isValidNode)

    val vertices = validNodes.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => ClumsyEdge(input(n.row)(n.col), n)).toSet
      ClumsyVertice(node = v, adjoiningNodes = edges)
    })

    val pathMap = validNodes.map(n => n -> (if (n.row == start._1 && n.col == start._2) ClumsyPath(null, 0) else ClumsyPath(null, Int.MaxValue)))
      .toMap.view.mapValues(v => (v, false)).toMap

    val verticesMap = vertices.map(v => v.node -> v).toMap
    val unvisitedNodes = vertices.map(_.node).toSet
    val visitedNodes = Set.empty[ClumsyNode]
    @tailrec
    def findShortestPath( pathMap: Map[ClumsyNode, (ClumsyPath, Boolean)], unvisitedNodes: Set[ClumsyNode], visitedNodes: Set[ClumsyNode]): Int = {
      println(unvisitedNodes.size)
      if (unvisitedNodes.nonEmpty) {
        val (currentNode, _) = pathMap.view.filterKeys(n => unvisitedNodes.contains(n)).minBy(_._2._1.distance)

        val newPathMap = pathMap ++ verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedNodes.contains(e.endNode) ||
            (pathMap(currentNode).previous != null && e.endNode.row == pathMap(currentNode).previous.row && e.endNode.col == pathMap(currentNode).previous.col))
          .filter(adj => {
            pathMap(adj.endNode).distance > pathMap(currentNode).distance + adj.distance
          })
          .map(adj => adj.endNode -> pathMap(adj.endNode).copy(distance = pathMap(currentNode).distance + adj.distance, previous = currentNode)).toMap
        findShortestPath(newPathMap, unvisitedNodes - currentNode, visitedNodes + currentNode)
      }
      else {
        pathMap.view.filterKeys(c => c.row == input.length - 1 && c.col == input.head.length - 1).values.map(_.distance).min
      }
    }

    //    while (unvisitedNodes.nonEmpty) {
    //      val currentNode = pathMap.view.filterKeys(n => unvisitedNodes.contains(n)).minBy(_._2.distance)._1
    //      if (currentNode.row == 1 && currentNode.col == 2) {
    //        println
    //      }
    //      verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedNodes.contains(e.endNode) ||
    //          (pathMap(currentNode).previous != null && e.endNode.row == pathMap(currentNode).previous.row && e.endNode.col == pathMap(currentNode).previous.col))
    //        .foreach(adj => {
    //          val adjPath = pathMap(adj.endNode)
    //          if (pathMap(adj.endNode).distance > pathMap(currentNode).distance + adj.distance)
    //            pathMap(adj.endNode) = adjPath.copy(distance = pathMap(currentNode).distance + adj.distance, previous = currentNode)
    //        })
    //      visitedNodes.add(currentNode)
    //      unvisitedNodes.remove(currentNode)
    //      println(unvisitedNodes.size)
    //    }
    //    val result = ArrayBuffer.fill(input.length, input.head.length)(0)
    //    var curr = pathMap.view.filterKeys(c => c.row == input.length - 1 && c.col == input.head.length - 1).minBy(_._2.distance)._1
    //    //    while (curr != start) {
    //    //      result(curr.row)(curr.col) = pathMap(curr).distance
    //    //      curr = pathMap(curr).previous
    //    //    }
    //    import extensions.StringExtensions.*
    //    result.map(_.map(_.toString.lpad(' ', 4)).mkString).foreach(println)
    //    pathMap.view.filterKeys(c => c.row == input.length - 1 && c.col == input.head.length - 1).values.map(_.distance).min
    findShortestPath(pathMap, unvisitedNodes, visitedNodes)
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}
