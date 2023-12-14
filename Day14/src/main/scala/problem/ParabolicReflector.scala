package problem

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object ParabolicReflector {

  private def readFile(filePath: String) = {
    ArrayBuffer(Using(Source.fromFile(filePath)) { file =>
      file
        .getLines().map(line => ArrayBuffer(line.toCharArray: _*)).toArray
    }.get: _*)
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

  private def moveCycle(input: ArrayBuffer[ArrayBuffer[Char]]) = {
    // North
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
    // West
    for row <- input.indices do
      var boundary = -1
      for col <- input(row).indices do
        if (input(row)(col) == '#') {
          boundary = col
        }
        else if (input(row)(col) == 'O' && boundary + 1 == col) {
          boundary += 1
        }
        else if (input(row)(col) == 'O') {
          input(row)(boundary + 1) = 'O'
          input(row)(col) = '.'
          boundary += 1
        }
    // South
    for col <- input.head.indices do
      var boundary = input.size
      for row <- input.indices.reverse do
        if (input(row)(col) == '#') {
          boundary = row
        }
        else if (input(row)(col) == 'O' && boundary - 1 == row) {
          boundary -= 1
        }
        else if (input(row)(col) == 'O') {
          input(boundary - 1)(col) = 'O'
          input(row)(col) = '.'
          boundary -= 1
        }
    // East
    for row <- input.indices do
      var boundary = input(row).size
      for col <- input(row).indices.reverse do
        if (input(row)(col) == '#') {
          boundary = col
        }
        else if (input(row)(col) == 'O' && boundary - 1 == col) {
          boundary -= 1
        }
        else if (input(row)(col) == 'O') {
          input(row)(boundary - 1) = 'O'
          input(row)(col) = '.'
          boundary -= 1
        }
    input
  }

  private def moveSouth(input: ArrayBuffer[ArrayBuffer[Char]]) = {
    val reversedInput = input.reverse
    moveNorth(reversedInput).reverse
  }

  private def moveWest(input: ArrayBuffer[ArrayBuffer[Char]]) = {
    input.map(row => {
      var boundary = -1
      for col <- row.indices do
        if (row(col) == '#') {
          boundary = col
        }
        else if (row(col) == 'O' && boundary + 1 == col) {
          boundary += 1
        }
        else if (row(col) == 'O') {
          row(boundary + 1) = 'O'
          row(col) = '.'
          boundary += 1
        }
      row
    })
  }

  private def moveEast(input: ArrayBuffer[ArrayBuffer[Char]]) = {
    val reversedInput = input.map(_.reverse)
    val movedInput = moveWest(reversedInput)
    movedInput.map(_.reverse)
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
    var input = readFile(filePath)
    for i <- 1 to 1000000000 do
//      println(i)
      input = moveCycle(input)
//    input.map(_.mkString).foreach(println)
    var multiplier = input.size
    input.map(_.count(_ == 'O')).map(c => {
      val r = c * multiplier
      multiplier -= 1
      r
    }).sum
  }
}