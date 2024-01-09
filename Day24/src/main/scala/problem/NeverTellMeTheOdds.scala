package problem

import extensions.StringExtensions.*

import scala.io.Source
import scala.math.*
import scala.util.Using

final case class Axis(x: BigDecimal, y: BigDecimal, z: BigDecimal)

final case class Hail(position: Axis, velocity: Axis)

object NeverTellMeTheOdds {


  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split('@') match
        case Array(position, velocity) => Hail(position.split(',') match
          case Array(x, y, z) => Axis(x.trim.toBD, y.trim.toBD, z.trim.toBD)
          , velocity.split(',') match
            case Array(x, y, z) => Axis(x.trim.toBD, y.trim.toBD, z.trim.toBD))).toSeq
  }.get


  private def isRightDirection(p: BigDecimal, v: BigDecimal, newP: BigDecimal) = {
    (v.sign == -1 && newP < p) || (v.sign == 0 && newP == p) || (v.sign == 1 && newP > p)
  }

  private def findCrossingHailStorms(input: Seq[Hail], minRange: BigDecimal, maxRange: BigDecimal) = {

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
    }
    )
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    findCrossingHailStorms(input, BigDecimal.valueOf(200000000000000.0), BigDecimal.valueOf(400000000000000.0))
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}
