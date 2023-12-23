package problem

import scala.io.Source
import scala.util.Using

object SandSlabs {

  final case class Coordinate(x: Int, y: Int, z: Int)

  final case class Brick(start: Coordinate, end: Coordinate)

  final case class Support(supporting: Seq[Brick], supportedBy: Seq[Brick])

  //  val cache = mutable.Map.empty[Brick, Support]

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.split('~').map(_.split(',') match
        case Array(x, y, z) => Coordinate(x.toInt, y.toInt, z.toInt)
      ) match
        case Array(start, end) => Brick(start, end)
      ).toSeq
  }.get

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    //    brickSupportMap.foreach(println)
//    input.sortBy(_.start.z).foreach(println)
    val movedBricks = input.sortBy(_.start.z).foldLeft(Seq.empty[Brick]
    )((sum, elem) => {
      var skip = false
      (elem.start.z - 1 to 1 by -1).foldLeft(elem)((b, e) => {
        val myCoordinates = Range.inclusive(b.start.x, b.end.x).flatMap(x => Range.inclusive(b.start.y, b.end.y).map(y =>
          (x, y))).toSet
        if (!skip && !sum.exists(o => {
          val otherCoordinates = Range.inclusive(o.start.x, o.end.x).flatMap(x => Range.inclusive(o.start.y, o.end.y).map(y =>
            (x, y))).toSet
          b.start.z - 1 == o.end.z && (myCoordinates intersect otherCoordinates).nonEmpty
        }))
        {
          b.copy(start = b.start.copy(z = e), b.end.copy(z = e + b.end.z - b.start.z))
        }
        else
        {
          skip = true
          b
        }
      }) +: sum
    })
//    println
//    movedBricks.sortBy(_.start.z).foreach(println)
    val brickSupportMap = movedBricks.map(b => {
      b -> Support(movedBricks.filter(o => o != b && (((b.start.x to b.end.x) intersect (o.start.x to o.end.x)).nonEmpty ||
        ((b.start.y to b.end.y) intersect (o.start.y to o.end.y)).nonEmpty) &&
        b.end.z + 1 == o.start.z), // supporting
        movedBricks.filter(o => o != b && (((b.start.x to b.end.x) intersect (o.start.x to o.end.x)).nonEmpty ||
          ((b.start.y to b.end.y) intersect (o.start.y to o.end.y)).nonEmpty) &&
          b.start.z - 1 == o.end.z) // supportedBy
      )
    }).toMap
//    println
//    brickSupportMap.foreach(println)
//    println
//    println
    brickSupportMap.count { case (_, s) =>
      val check = !s.supporting.exists(b =>
        brickSupportMap(b).supportedBy.size == 1)
      //      println(s"$br -> $s and check is $check")

      check
    }
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    0
  }
}