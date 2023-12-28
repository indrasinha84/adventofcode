package problem

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object HauntedWasteland {

  final case class Destination(left: String, right: String)

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().toSeq match
      case first :: rest =>
        (first.toCharArray,
          rest.filterNot(_.isBlank).map(_.replace(" ", "").split('=') match
            case Array(source, destination) => (source,
              destination.substring(1, destination.length - 1).split(',') match
                case Array(left, right) => Destination(left, right))).toMap)
  }.get

  def problem1(filePath: String): Int = {
    val (steps, directionMap) = readFile(filePath)
    var stepsTaken = 0
    var nextStep = 0
    var nextPlace = "AAA"
    while (nextPlace != "ZZZ") {
      nextPlace = steps(nextStep) match
        case 'R' => directionMap(nextPlace).right
        case 'L' => directionMap(nextPlace).left
      stepsTaken += 1
      nextStep = if (nextStep + 1 == steps.length) 0 else nextStep + 1

    }
    stepsTaken
  }

  def lcm(list: Seq[Long]): Long = list.foldLeft(1: Long) { (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs }


  def problem2(filePath: String): Long = {
    val (steps, directionMap) = readFile(filePath)
    val nextPlaces = directionMap.keys.filter(_.last == 'A').map(n => (n, n.last)).toSeq

    @tailrec
    def findZ(nextPlaces: Seq[(String, Char)], nextStep: Int, cnt: Long): Long = {

      if (!nextPlaces.exists(_._2 != 'Z')) {
        cnt
      }
      else {
        steps(nextStep) match
          case 'R' => findZ(nextPlaces.map(nextPlace => {
            val next = directionMap(nextPlace._1).right
            (next, next.last)
          }), if (nextStep + 1 == steps.length) 0 else nextStep + 1, cnt + 1)
          case 'L' => findZ(nextPlaces.map(nextPlace => {
            val next = directionMap(nextPlace._1).left
            (next, next.last)
          }), if (nextStep + 1 == steps.length) 0 else nextStep + 1, cnt + 1)
      }

    }
    val stepsForEach = nextPlaces.map(n => findZ(Seq(n), 0, 0L))
    lcm(stepsForEach)
  }
}