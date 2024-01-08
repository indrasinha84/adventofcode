package problem

import scala.io.Source
import scala.math.*
import scala.util.Using

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

  var mergeCount = 0
  var mergeCountWithinRange = 0

  private def findCrossingHailStorms(input: Seq[Hail], minRange: Double, maxRange: Double) = {

    input.combinations(2).map(combination => {
      val nodeFrom = combination.minBy(c => (c.position.x, c.position.y))
      val nodeTo = combination.maxBy(c => (c.position.x, c.position.y))
      val a = sqrt(pow(nodeTo.position.y - nodeFrom.position.y, 2) + pow(nodeTo.position.x - nodeFrom.position.x, 2))
      val nodeFromAngleWithSign = atan(nodeFrom.velocity.y / nodeFrom.velocity.x)
      val nodeToAngleWithSign = atan(nodeTo.velocity.y / nodeTo.velocity.x)
      {
        val B = (atan2(nodeTo.position.y - nodeFrom.position.y, nodeTo.position.x - nodeFrom.position.x) -
          atan2(nodeFrom.velocity.y, nodeFrom.velocity.x)).abs
        val C = (atan2(nodeFrom.position.y - nodeTo.position.y, nodeFrom.position.x - nodeTo.position.x)
          - atan2(nodeTo.velocity.y, nodeTo.velocity.x)).abs
        val A =  toRadians(180 - toDegrees(B) - toDegrees(C))
        val c = sin(C) / sin(A) * a
        val b = sin(B) / sin(A) * a
        val nodeFromx = nodeFrom.position.x + (cos(nodeFromAngleWithSign.abs) * c * nodeFrom.velocity.x.sign) //nodeFrom.position.x + sin(toRadians(90 - nodeFromAngle)).abs * c * nodeFrom.velocity.x.sign
        val nodeFromy = nodeFrom.position.y + (sin(nodeFromAngleWithSign.abs) * c * nodeFrom.velocity.y.sign) //nodeFrom.position.y + sin(toRadians(nodeFromAngle)).abs * c * nodeFrom.velocity.y.sign
        val nodeTox = nodeTo.position.x + (cos(nodeToAngleWithSign.abs) * b * nodeTo.velocity.x.sign) //nodeTo.position.x + sin(toRadians(90 - nodeToAngle)).abs * b * nodeTo.velocity.x.sign
        val nodeToy = nodeTo.position.y + (sin(nodeToAngleWithSign.abs) * b * nodeTo.velocity.y.sign) //nodeTo.position.y + sin(toRadians(nodeToAngle)).abs * b * nodeTo.velocity.y.sign

        val distanceBetweenIntersections = sqrt(pow(nodeToy - nodeFromy, 2) + pow(nodeTox - nodeFromx, 2))
        if (distanceBetweenIntersections > 0.0001) {
          0 // Did not merge in future
        }
        else if (nodeFromx >= minRange && nodeFromx <= maxRange && nodeFromy >= minRange && nodeFromy <= maxRange) {
          println
          1 //Not within range.
        }
        else {
          0
        }
      }
    }
    ).sum
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val x = findCrossingHailStorms(input, 7.0, 27.0)
    //    println(mergeCount)
    //    println(mergeCountWithinRange)
    x
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}
