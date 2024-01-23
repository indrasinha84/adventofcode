package problem

import extensions.StringExtensions.*

import scala.io.Source
import scala.math.*
import scala.util.Using

final case class Axis(x: BigDecimal, y: BigDecimal, z: BigDecimal)

final case class Hail(position: Axis, velocity: Axis)

object NeverTellMeTheOdds {

  private val Zero = BigDecimal.valueOf(0L)

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

  import extensions.StringExtensions.*

  def findDeterminant(numArray: Array[Array[BigDecimal]]): BigDecimal = {
    numArray.indices.map(i => {
      val newDeterminant = if (numArray.length == 2) {
        numArray(1)(1 - i)
      } else {
        findDeterminant(numArray.tail.map(_.zipWithIndex.filter(_._2 != i).map(_._1).toArray))
      }
      numArray(0)(i) * newDeterminant
    }).foldLeft((Zero, 1))((a, b) => (a._1 + (b * a._2), a._2 * -1))._1
  }


  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    def vx(i: Int) = {
      input(i).velocity.x
    }
    def vy(i: Int) = {
      input(i).velocity.y
    }
    def vz(i: Int) = {
      input(i).velocity.z
    }
    def px(i: Int) = {
      input(i).position.x
    }
    def py(i: Int) = {
      input(i).position.y
    }
    def pz(i: Int) = {
      input(i).position.z
    }
    val DArray = Array(
      Array(vy(0) - vy(1), vx(1) - vx(0), Zero, py(1) - py(0), px(0) - px(1), Zero),
      Array(vz(0) - vz(1), Zero, vx(1) - vx(0), pz(1) - pz(0), Zero, px(0) - px(1)),
      Array(vy(0) - vy(2), vx(2) - vx(0), Zero, py(2) - py(0), px(0) - px(2), Zero),
      Array(vz(0) - vz(2), Zero, vx(2) - vx(0), pz(2) - pz(0), Zero, px(0) - px(2)),
      Array(vy(0) - vy(3), vx(3) - vx(0), Zero, py(3) - py(0), px(0) - px(3), Zero),
      Array(vz(0) - vz(3), Zero, vx(3) - vx(0), pz(3) - pz(0), Zero, px(0) - px(3))
    )

    val RightCoefficient = Array(
      px(0) * vy(0) - py(0) * vx(0) - px(1) * vy(1) + py(1) * vx(1),
      px(0) * vz(0) - pz(0) * vx(0) - px(1) * vz(1) + pz(1) * vx(1),
      px(0) * vy(0) - py(0) * vx(0) - px(2) * vy(2) + py(2) * vx(2),
      px(0) * vz(0) - pz(0) * vx(0) - px(2) * vz(2) + pz(2) * vx(2),
      px(0) * vy(0) - py(0) * vx(0) - px(3) * vy(3) + py(3) * vx(3),
      px(0) * vz(0) - pz(0) * vx(0) - px(3) * vz(3) + pz(3) * vx(3)
    )
    val transposedD = DArray.transpose
    val DxArray = (RightCoefficient +: transposedD.tail).transpose
    val DyArray = ((transposedD.take(1) :+ RightCoefficient) ++ transposedD.takeRight(4)).transpose
    val DzArray = ((transposedD.take(2) :+ RightCoefficient) ++ transposedD.takeRight(3)).transpose
    val D = findDeterminant(DArray)
    val Dx = findDeterminant(DxArray)
    val Dy = findDeterminant(DyArray)
    val Dz = findDeterminant(DzArray)
    val x = Dx / D
    val y = Dy / D
    val z = Dz / D
    (x + y + z).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
  }
}
