package problem

import scala.io.Source
import scala.util.Using
import problem.Direction.*

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer


private enum Direction:
  case Up
  case Down
  case Right
  case Left

final case class Block(row: Int, col: Int, direction: Direction, allowedMoves: Seq[Direction], remainingMoves: Int)

object ClumsyCrucible {
  type MoveBlock = (ArrayBuffer[ArrayBuffer[Set[Path]]], Direction, Block) => Option[Block]
  type Path = (Int, Block)

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.toCharArray.toSeq).toSeq
  }.get

  private def moveBlockFunc(rowLength: Int, colLength: Int)(blockMap: ArrayBuffer[ArrayBuffer[Set[Path]]], direction: Direction, currentBlock: Block) = {
    val remainingMoves = if (currentBlock.direction == direction) currentBlock.remainingMoves - 1 else 2
    val nextBlock = direction match
      case Direction.Up => Block(currentBlock.row - 1, currentBlock.col, direction, Seq(Left, Right, Up), remainingMoves)
      case Direction.Down => Block(currentBlock.row + 1, currentBlock.col, direction, Seq(Left, Right, Down), remainingMoves)
      case Direction.Right => Block(currentBlock.row, currentBlock.col + 1, direction, Seq(Right, Up, Down), remainingMoves)
      case Direction.Left => Block(currentBlock.row, currentBlock.col - 1, direction, Seq(Left, Up, Down), remainingMoves)
    Option.when((remainingMoves >= 0 && nextBlock.row >= 0 && nextBlock.row < rowLength && nextBlock.col >= 0 && nextBlock.col < colLength) &&
      !(currentBlock.row == rowLength - 1 && currentBlock.col == colLength - 1)
    )(nextBlock)
  }

  def findPaths(input: Array[Array[Int]], moveBlock: MoveBlock, paths: Seq[Path]): Int = {
    var shortestPath = Int.MaxValue
    var results = paths
    val blockMap = ArrayBuffer.fill(input.length)(ArrayBuffer.fill(input.head.length)(Set.empty[(Int, Block)]))
    while (results.nonEmpty) {
      results = results.flatMap(path => {
        val lastBlock = path._2
        blockMap(lastBlock.row)(lastBlock.col) = blockMap(lastBlock.row)(lastBlock.col) + ((path._1, lastBlock))
        val nextMoves = lastBlock.allowedMoves.flatMap(next => moveBlock(blockMap, next, lastBlock) match
          case Some(value) =>
            if (value.row == input.length - 1 && value.col == input.head.length - 1) {
              if ((path._1 + input(value.row)(value.col)) < shortestPath) {
                shortestPath = path._1 + input(value.row)(value.col)
              }
              Seq.empty[Path]
            }
            else if (blockMap(value.row)(value.col).map(_._1).minOption.getOrElse(Int.MaxValue) > path._1 + input(value.row)(value.col)) {
              Seq((path._1 + input(value.row)(value.col), value))
            }
            else {
              Seq.empty[Path]
            }
          case _ => Seq.empty[Path]
        )
        nextMoves
      })
    }
    shortestPath
  }


  def problem1(filePath: String): Int = {
    //    val input = readFile(filePath)
    //    val rowLength = input.length
    //    val colLength = input.head.length
    //    val moveBlock = moveBlockFunc(rowLength, colLength)
    //    findPaths(input, moveBlock, Seq((0, Block(0, 0, Right, Seq(Right, Down), 2))
    ////      , (0, Seq.empty, Block(0, 0, Down, Seq(Right, Down), 2))
    //    ))
    0
  }

  def problem2(filePath: String): Iterator[String] = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
    }.get
  }
}
