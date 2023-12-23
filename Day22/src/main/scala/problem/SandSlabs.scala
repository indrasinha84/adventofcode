package problem

import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.*

object SandSlabs {

  final case class Coordinate(x: Int, y: Int, z: Int)

  final case class Brick(num: Int, start: Coordinate, end: Coordinate)

  final case class Support(supporting: Seq[Brick], supportedBy: Seq[Brick])
  
  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.split('~').map(_.split(',') match
        case Array(x, y, z) => Coordinate(x.toInt, y.toInt, z.toInt)
      ) match
        case Array(start, end) =>
          Brick(0, start, end)
      ).toSeq.sortBy(b => (b.start.z, b.start.x, b.start.y)).zipWithIndex.map(bi => bi._1.copy(num = bi._2 + 1))
  }.get

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    val movedBricks = input.sortBy(_.start.z).foldLeft(Seq.empty[Brick]
    )((sum, elem) => {
      var movedBrick = elem
      breakable {
        for e <- elem.start.z - 1 to 1 by -1 do {
          val myCoordinates = Range.inclusive(movedBrick.start.x, movedBrick.end.x).flatMap(x => Range.inclusive(movedBrick.start.y, movedBrick.end.y).map(y =>
            (x, y))).toSet
          if (!sum.exists(o => {
            val otherCoordinates = Range.inclusive(o.start.x, o.end.x).flatMap(x => Range.inclusive(o.start.y, o.end.y).map(y =>
              (x, y))).toSet
            movedBrick.start.z - 1 == o.end.z && (myCoordinates intersect otherCoordinates).nonEmpty
          })) {
            movedBrick = movedBrick.copy(start = movedBrick.start.copy(z = e), movedBrick.end.copy(z = e + movedBrick.end.z - movedBrick.start.z))
          }
          else {
            break
          }
        }
      }
      sum :+ movedBrick
    })
    val brickSupportMap = movedBricks.map(b => {
      val movedBrick = b
      val myCoordinates = Range.inclusive(movedBrick.start.x, movedBrick.end.x).flatMap(x => Range.inclusive(movedBrick.start.y, movedBrick.end.y).map(y =>
        (x, y))).toSet
      b -> Support(movedBricks.filter(o => {
        val otherCoordinates = Range.inclusive(o.start.x, o.end.x).flatMap(x => Range.inclusive(o.start.y, o.end.y).map(y =>
          (x, y))).toSet
        o != b && (myCoordinates intersect otherCoordinates).nonEmpty && b.end.z + 1 == o.start.z
      }), // supporting
        movedBricks.filter(o => {
          val otherCoordinates = Range.inclusive(o.start.x, o.end.x).flatMap(x => Range.inclusive(o.start.y, o.end.y).map(y =>
            (x, y))).toSet
          o != b && (myCoordinates intersect otherCoordinates).nonEmpty &&
            b.start.z - 1 == o.end.z
        }) // supportedBy
      )
    }).toMap
    brickSupportMap.count { case (_, s) =>
      val check = !s.supporting.exists(b =>
        brickSupportMap(b).supportedBy.size == 1)
      check
    }
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}