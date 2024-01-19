package problem

import extensions.StringExtensions.*

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.io.Source
import scala.math.*
import scala.util.Using
import scala.xml.NodeSeq

final case class Axis(x: Double, y: Double, z: Double)

final case class Hail(position: Axis, velocity: Axis)

object NeverTellMeTheOdds {


  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split('@') match
        case Array(position, velocity) => Hail(position.split(',') match
          case Array(x, y, z) => Axis(x.trim.toDouble, y.trim.toDouble, z.trim.toDouble)
          , velocity.split(',') match
            case Array(x, y, z) => Axis(x.trim.toDouble, y.trim.toDouble, z.trim.toDouble))).toSeq
  }.get


  private def isRightDirection(p: Double, v: Double, newP: Double) = {
    (v.sign == -1 && newP < p) || (v.sign == 0 && newP == p) || (v.sign == 1 && newP > p)
  }

  private def findCrossingHailStorms(input: Seq[Hail], minRange: Long, maxRange: Long) = {
    input.combinations(2).count(combination => {
      val nodeFrom = combination.minBy(c => (c.position.x, c.position.y))
      val nodeTo = combination.maxBy(c => (c.position.x, c.position.y))
      val px1 = nodeFrom.position.x
      val py1 = nodeFrom.position.y
      val vx1 = nodeFrom.velocity.x
      val vy1 = nodeFrom.velocity.y
      val px2 = nodeTo.position.x
      val py2 = nodeTo.position.y
      val vx2 = nodeTo.velocity.x
      val vy2 = nodeTo.velocity.y
      val c1 = px1 * vy1 - py1 * vx1
      val c2 = px2 * vy2 - py2 * vx2
      Option.when((vy1 * vx2 - vx1 * vy2) != 0 && vy1 != 0)((vy2 * c1 - vy1 * c2) / (vy1 * vx2 - vx1 * vy2)).map(y => ((c1 + vx1 * y) / vy1, y))
        .exists { case (x, y) => x >= minRange && x <= maxRange && y >= minRange && y <= maxRange &&
          isRightDirection(px1, vx1, x) && isRightDirection(py1, vy1, y) && isRightDirection(px2, vx2, x) && isRightDirection(py2, vy2, y)
        }
    })
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    findCrossingHailStorms(input, 200000000000000L, 400000000000000L)
  }

  def moveAxis(inp: (Hail, Axis)): (Hail, Axis) = inp match {
    case (h, a) => (h, a.copy(a.x + h.velocity.x, a.y + h.velocity.y, a.z + h.velocity.z))
  }

  private def distanceBetween(from: Axis, to: Axis) = {
    sqrt(pow(from.x - to.x, 2) + pow(from.y - to.y, 2) + pow(from.z - to.z, 2))
  }

  private def calculateForFirstRound(input: Seq[(Hail, Axis)]): Seq[(Seq[(Hail, Axis)], (Hail, Axis), (Hail, Axis))] = {
    val first = input
    val second = first.map(moveAxis)
    val fAndS = first.zip(second)
    val lowerLimit = -1000
    val upperLimit = 1000
    val rangeToCheck = lowerLimit to upperLimit
    fAndS.flatMap({ case (boundary, _) =>
      rangeToCheck.flatMap(x => {
        rangeToCheck.flatMap(y => {
          rangeToCheck.flatMap(z => {
            val stone = (Hail(boundary._2, Axis(x, y, z)), boundary._2)
            val start = stone._2
            val parallelPresent = fAndS.exists({ case (f, _) => stone._1.velocity == f._1.velocity && f._2 != start })
            val increasingPresent = fAndS.exists({ case (f, s) => (distanceBetween(f._2,start) < distanceBetween(s._2, moveAxis(stone)._2)) && f._2 != start })
            Option.when(!parallelPresent && !increasingPresent)({
              (input.filterNot(_._1 == boundary._1).map(moveAxis), moveAxis(boundary), moveAxis(stone))
            })
          })
        })
      })
    })
  }

  private def calculateForOtherRounds(dataForCalc: Seq[(Seq[(Hail, Axis)], (Hail, Axis), (Hail, Axis))]): Seq[(Seq[(Hail, Axis)], (Hail, Axis), (Hail, Axis))] = {
    println("next round called")
    dataForCalc.flatMap({ case (input, boundary, stone) =>
      val first = input
      val second = first.map(moveAxis)
      val fAndS = first.zip(second)
      var v = stone._1.velocity.x
      val start = stone._2
      val parallelPresent = fAndS.exists({ case (f, _) => v == f._1.velocity.x && f._2 != start })
      val increasingPresent = fAndS.exists({ case (f, s) => (distanceBetween(f._2,start) < distanceBetween(s._2, moveAxis(stone)._2)) && f._2 != start })
      Option.when(!parallelPresent && !increasingPresent)((input.filterNot(_._2 == stone._2).map(moveAxis), moveAxis(boundary), moveAxis(stone)))
    })
  }

  @tailrec
  private def findVelocity(result: Seq[(Seq[(Hail, Axis)], (Hail, Axis), (Hail, Axis))]): Seq[(Seq[(Hail, Axis)], (Hail, Axis), (Hail, Axis))] = {
    if (!result.exists(_._1.nonEmpty)) {
      result
    }
    else {
      val next = if (result.exists(_._3 == null)) calculateForFirstRound(result.head._1) else calculateForOtherRounds(result)
      findVelocity(next)
    }
  }

  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    val inputWithCurrentPositions = input.map(h => (h, h.position)).sortBy(_._1.position.x)
    val result = findVelocity(Seq((inputWithCurrentPositions.map(moveAxis), null, null)))
    result.map({ case (_, _, (stone, _)) => stone.position.x - stone.velocity.x + stone.position.y - stone.velocity.y + stone.position.z - stone.velocity.z }).headOption.getOrElse(0.0).toLong
  }
}
