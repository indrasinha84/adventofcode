package problem

import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.*

object SandSlabs {

  final case class Coordinate(x: Int, y: Int, z: Int)

  final case class Brick(num: Int, start: Coordinate, end: Coordinate, pairs: Set[(Int, Int)])

  final case class Support(supporting: Seq[Brick], supportedBy: Seq[Brick])

  private def createPairs(start: Coordinate, end: Coordinate) = Range.inclusive(start.x, end.x).flatMap(x => Range.inclusive(start.y, end.y).map(y => (x, y))).toSet

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.split('~').map(_.split(',') match
        case Array(x, y, z) => Coordinate(x.toInt, y.toInt, z.toInt)
      ) match
        case Array(start, end) => Brick(0, start, end, createPairs(start, end))
      ).toSeq.sortBy(b => (b.start.z, b.start.x, b.start.y)).zipWithIndex.map(bi => bi._1.copy(num = bi._2 + 1))
  }.get

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val movedBricks = input.sortBy(_.start.z).foldLeft(Seq.empty[Brick]
    )((sum, elem) => {
      var movedBrick = elem
      breakable {
        for e <- elem.start.z - 1 to 1 by -1 do {
          if (!sum.exists(o => movedBrick.start.z - 1 == o.end.z && (movedBrick.pairs intersect o.pairs).nonEmpty)) {
            val newStart = movedBrick.start.copy(z = e)
            val newEnd = movedBrick.end.copy(z = e + movedBrick.end.z - movedBrick.start.z)
            movedBrick = movedBrick.copy(start = newStart, newEnd, createPairs(newStart, newEnd))
          }
          else {
            break
          }
        }
      }
      sum :+ movedBrick
    })
    val brickSupportMap = movedBricks.map(b => {
      b -> Support(movedBricks.filter(o => o != b && (b.pairs intersect o.pairs).nonEmpty && b.end.z + 1 == o.start.z), // supporting
        movedBricks.filter(o => o != b && (b.pairs intersect o.pairs).nonEmpty && b.start.z - 1 == o.end.z) // supportedBy
      )
    }).toMap
    brickSupportMap.count { case (_, s) => !s.supporting.exists(b => brickSupportMap(b).supportedBy.size == 1) }
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}