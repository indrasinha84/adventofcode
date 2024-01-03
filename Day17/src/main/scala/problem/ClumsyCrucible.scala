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

object ClumsyCrucible {
  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.map(_.toString.toInt).toSeq).toSeq
  }.get

  private def isStepsAllowedF(stepsMin: Int, stepsMax: Int)(oldNode: ClumsyNode, newNode: ClumsyNode) =
    (oldNode.direction == newNode.direction && newNode.steps < stepsMax) || (oldNode.direction != newNode.direction && oldNode.steps >= stepsMin)

  private def isValidNodeF(rowLength: Int, colLength: Int, stepsMax: Int)(p: ClumsyNode) = p.row >= 0 && p.row < rowLength && p.col >= 0 && p.col < colLength && p.steps < stepsMax


  def findPath(isValidNode: ClumsyNode => Boolean, isStepsAllowed: (ClumsyNode, ClumsyNode) => Boolean)(position: ClumsyNode): Seq[ClumsyNode] = {
    (position.row, position.col) match
      case (r, c) =>
        Seq(ClumsyNode(r, c + 1, "Right", if (position.direction == "Right") position.steps + 1 else 0),
          ClumsyNode(r, c - 1, "Left", if (position.direction == "Left") position.steps + 1 else 0),
          ClumsyNode(r + 1, c, "Down", if (position.direction == "Down") position.steps + 1 else 0),
          ClumsyNode(r - 1, c, "Up", if (position.direction == "Up") position.steps + 1 else 0)
        ).filter(n => isValidNode(n) && isStepsAllowed(position, n))

  }

  private def nodeToClumsyNodeF(stepsMax: Int)(n: Node) = {
    Seq.range(0, stepsMax).flatMap(i => Seq("Right", "Left", "Up", "Down").map(d => ClumsyNode(n.row, n.col, d, i)))
  }

  private def calculateResults(input: Seq[Seq[Int]], stepsMin: Int, stepsMax: Int) = {
    val isValidNode = isValidNodeF(input.length, input.head.length, stepsMax)
    val isStepsAllowed = isStepsAllowedF(stepsMin, stepsMax)
    val nodeToClumsyNode = nodeToClumsyNodeF(stepsMax)
    val validNodes = input.toGraphNodes.flatMap(nodeToClumsyNode).filter(isValidNode).filter(n => input(n.row)(n.col) != '.')
    val start = (0, 0)
    val end = (input.length - 1, input.head.length - 1)
    val pathFinder = findPath(isValidNode, isStepsAllowed)

    val vertices = validNodes.sortBy(n => (n.row, n.col)).map(v => {
      val edges = pathFinder(v).map(n => ClumsyEdge(input(n.row)(n.col), n)).toSet
      ClumsyVertice(node = v, adjoiningNodes = edges)
    })

    val pathMap1 = validNodes.map(n => n -> (if (n.row == start._1 && n.col == start._2) ClumsyPath(null, 0) else ClumsyPath(null, Int.MaxValue))).toMap

    val unvisitedMap = mutable.Map.from(pathMap1)

    val visitedSet = mutable.Set.empty[ClumsyNode]
    val verticesMap = vertices.map(v => v.node -> v).toMap
    val shortestPaths = mutable.Set.empty[Int]

    @tailrec
    def findShortestPath(): Int = {
      if (unvisitedMap.nonEmpty) {
        val minValue = unvisitedMap.map(_._2.distance).min
        val currentNodes = unvisitedMap.filter(_._2.distance == minValue)
        currentNodes.foreach({ case (currentNode, currentPath) =>
          verticesMap(currentNode).adjoiningNodes.filterNot(e => visitedSet.contains(e.endNode) ||
              (currentPath.previous != null && e.endNode.row == currentPath.previous.row && e.endNode.col == currentPath.previous.col))
            .filter(adj => {
              unvisitedMap(adj.endNode).distance > currentPath.distance + adj.distance
            })
            .foreach(adj => {
              unvisitedMap(adj.endNode) = unvisitedMap(adj.endNode).copy(distance = currentPath.distance + adj.distance, previous = currentNode)
            }

            )
          visitedSet.addOne(currentNode)
          if (currentNode.row == end._1 && currentNode.col == end._2) {
            shortestPaths ++= unvisitedMap.view.filterKeys(c => c.row == input.length - 1 && c.col == input.head.length - 1).values.map(_.distance).filterNot(_ == 0)
          }
          unvisitedMap.remove(currentNode)
        })

        findShortestPath()
      }
      else {
        shortestPaths.min
      }
    }

    findShortestPath()
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    calculateResults(input, 0, 3)

  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    calculateResults(input, 3, 10)
  }
}
