package problem

import problem.Direction.*

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

private enum Direction:
  case Up
  case Down
  case Right
  case Left

final case class Beam(row: Int, col: Int, direction: Direction, sing: Char)

object TheFloorWillBeLava {


  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines().map(_.toCharArray).toArray
  }.get


  private def moveBeamFunc(rowLength: Int, colLength: Int, beamMap: Array[Array[Seq[Beam]]]) = (row: Int, col: Int, direction: Direction) => {
    val (nextRow, nextCol) = direction match
      case Direction.Up => (row - 1, col)
      case Direction.Down => (row + 1, col)
      case Direction.Right => (row, col + 1)
      case Direction.Left => (row, col - 1)
    if (nextRow >= 0 && nextRow < rowLength && nextCol >= 0 && nextCol < colLength &&
      !beamMap(nextRow)(nextCol).map(_.direction).contains(direction)) Some((nextRow, nextCol)) else None
  }


  private def addToMapFunc(beamMap: Array[Array[Seq[Beam]]]) = (beam: Beam) => beamMap(beam.row)(beam.col) = beamMap(beam.row)(beam.col) :+ beam

  private def moveAndPushIfValidFunc(rowLength: Int, colLength: Int, beamStack: mutable.Stack[Beam], beamMap: Array[Array[Seq[Beam]]], input: Array[Array[Char]]) = (beam: Beam, direction: Direction) => {
    val addToMap = addToMapFunc(beamMap)
    val moveBeam = moveBeamFunc(rowLength, colLength, beamMap)
    addToMap(beam)
    moveBeam(beam.row, beam.col, direction) match
      case Some(value) => beamStack.push(Beam(value._1, value._2, direction, input(value._1)(value._2)))
      case None =>
  }


  private def getEnergizedTilesCountFunc(input: Array[Array[Char]])(start: Beam) = {
    val rowLength = input.length
    val colLength = input.head.length
    val beamMap = Array.fill(rowLength)(Array.fill(colLength)(Seq.empty[Beam]))
    val beamStack = mutable.Stack[Beam](start)
    val moveAndPushIfValid = moveAndPushIfValidFunc(rowLength, colLength, beamStack, beamMap, input)
    while (beamStack.nonEmpty) {
      val cur = beamStack.pop()
      cur match
        case Beam(_, _, Left, '.') =>
          moveAndPushIfValid(cur, Left)
        case Beam(_, _, Left, '\\') => moveAndPushIfValid(cur, Up)
        case Beam(_, _, Left, '/') => moveAndPushIfValid(cur, Down)
        case Beam(_, _, Left, '|') =>
          moveAndPushIfValid(cur, Up)
          moveAndPushIfValid(cur, Down)
        case Beam(_, _, Left, '-') => moveAndPushIfValid(cur, Left)
        case Beam(_, _, Right, '.') => moveAndPushIfValid(cur, Right)
        case Beam(_, _, Right, '\\') => moveAndPushIfValid(cur, Down)
        case Beam(_, _, Right, '/') => moveAndPushIfValid(cur, Up)
        case Beam(_, _, Right, '|') =>
          moveAndPushIfValid(cur, Up)
          moveAndPushIfValid(cur, Down)
        case Beam(_, _, Right, '-') => moveAndPushIfValid(cur, Right)

        case Beam(_, _, Down, '.') => moveAndPushIfValid(cur, Down)
        case Beam(_, _, Down, '\\') => moveAndPushIfValid(cur, Right)
        case Beam(_, _, Down, '/') => moveAndPushIfValid(cur, Left)
        case Beam(_, _, Down, '|') => moveAndPushIfValid(cur, Down)
        case Beam(_, _, Down, '-') =>
          moveAndPushIfValid(cur, Right)
          moveAndPushIfValid(cur, Left)
        case Beam(_, _, Up, '.') => moveAndPushIfValid(cur, Up)
        case Beam(_, _, Up, '\\') => moveAndPushIfValid(cur, Left)
        case Beam(_, _, Up, '/') => moveAndPushIfValid(cur, Right)
        case Beam(_, _, Up, '|') => moveAndPushIfValid(cur, Up)
        case Beam(_, _, Up, '-') =>
          moveAndPushIfValid(cur, Right)
          moveAndPushIfValid(cur, Left)
        case _ =>
    }
    beamMap.flatMap(_.map(s => if (s.nonEmpty) 1 else 0)).sum
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    getEnergizedTilesCountFunc(input)(Beam(0, 0, Right, input(0)(0)))
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val rowLength = input.length
    val colLength = input.head.length
    val getEnergizedTilesCount = getEnergizedTilesCountFunc(input)
    val startList = input.indices.flatMap(r => Seq(Beam(r, 0, Right, input(r)(0)), Beam(r, colLength - 1, Left, input(r)(colLength - 1)))
    ) ++ input.head.indices.flatMap(c => Seq(Beam(0, c, Down, input(0)(c)), Beam(rowLength - 1, c, Up, input(rowLength - 1)(c))))
    startList.map(getEnergizedTilesCount).max
  }
}