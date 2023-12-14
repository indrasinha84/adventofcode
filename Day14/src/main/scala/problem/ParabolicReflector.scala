package problem

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

// TODO Remove ArrayBuffer
object ParabolicReflector {

  var cache = mutable.Map.empty[String, (String, Array[Array[Char]])]

  private def readFile(filePath: String) = {
    ArrayBuffer(Using(Source.fromFile(filePath)) { file =>
      file
        .getLines().map(line => ArrayBuffer(line.toCharArray: _*)).toArray
    }.get: _*)
  }

  private def readFile2(filePath: String) = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines().map(line => line.toCharArray).toArray
    }.get
  }

  private def moveNorth(input: ArrayBuffer[ArrayBuffer[Char]]) = {
    for col <- input.head.indices do
      var boundary = -1
      for row <- input.indices do
        if (input(row)(col) == '#') {
          boundary = row
        }
        else if (input(row)(col) == 'O' && boundary + 1 == row) {
          boundary += 1
        }
        else if (input(row)(col) == 'O') {
          input(boundary + 1)(col) = 'O'
          input(row)(col) = '.'
          boundary += 1
        }
    input
  }

  @tailrec
  private def moveCycle(rowSize: Int, columnSize: Int, cnt: Int, inputParam: (String, Array[Array[Char]])): Array[Array[Char]] = {
    if (cnt == 0)
      inputParam._2
    else {
      moveCycle(rowSize, columnSize, cnt - 1, cache.getOrElseUpdate(inputParam._1, {
        val input = ArrayBuffer(inputParam._2.map(a => ArrayBuffer(a: _*)): _*)
        // North
        for col <- 0 until columnSize do
          var boundary = -1
          for row <- 0 until rowSize do
            boundary = input(row)(col) match {
              case '#' => row
              case 'O' if boundary + 1 == row => boundary + 1
              case 'O' =>
                input(boundary + 1)(col) = 'O'
                input(row)(col) = '.'
                boundary + 1
              case _ => boundary
            }
        // West
        for row <- 0 until rowSize do
          var boundary = -1
          val current = input(row)
          for col <- 0 until columnSize do
            boundary = current(col) match {
              case '#' => col
              case 'O' if boundary + 1 == col => boundary + 1
              case 'O' =>
                input(row)(boundary + 1) = 'O'
                input(row)(col) = '.'
                boundary + 1
              case _ => boundary
            }
          current.toArray
        // South
        for col <- 0 until columnSize do
          var boundary = rowSize
          for row <- rowSize - 1 to 0 by -1 do
            boundary = input(row)(col) match {
              case '#' => row
              case 'O' if boundary - 1 == row => boundary - 1
              case 'O' =>
                input(boundary - 1)(col) = 'O'
                input(row)(col) = '.'
                boundary - 1
              case _ => boundary
            }
        //     East
        for row <- 0 until rowSize do
          var boundary = columnSize
          for col <- columnSize - 1 to 0 by -1 do
            boundary = input(row)(col) match {
              case '#' => col
              case 'O' if boundary - 1 == col => boundary - 1
              case 'O' =>
                input(row)(boundary - 1) = 'O'
                input(row)(col) = '.'
                boundary - 1
              case _ => boundary
            }
        (input.map(_.mkString).mkString, input.map(_.toArray).toArray)
      }))
    }
  }


  def problem1(filePath: String): Int = {
    val input = moveNorth(readFile(filePath))
    var multiplier = input.size
    input.map(_.count(_ == 'O')).map(c => {
      val r = c * multiplier
      multiplier -= 1
      r
    }).sum
  }


  def problem2(filePath: String): Int = {
    var input = readFile2(filePath)
    input = moveCycle(input.length, input.head.length, 1000000000, (input.map(_.mkString).mkString, input))
    var multiplier = input.length
    input.map(_.count(_ == 'O')).map(c => {
      val r = c * multiplier
      multiplier -= 1
      r
    }).sum
  }
}