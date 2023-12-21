package problem

import scala.io.Source
import scala.math.abs
import scala.util.Using


final case class DigPlan(direction: Char, steps: Long, color: String)

final case class Block(row: Long, col: Long, digPlan: DigPlan, endRow: Long, endCol: Long)

object LavaductLagoon {

  private def readFile(filePath: String) = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(_.split(" ") match
          case Array(direction, steps, color) => DigPlan(direction.head, steps.toLong, color)
        ).toSeq
    }.get
  }

  private def buildLavaPool(input: Seq[DigPlan]): Long = {
    val blocks = input.foldLeft(Seq.empty[Block])((sum, elem) => {
      (sum, elem.direction) match {
        case (Nil, 'R') => Seq(Block(0, 0, elem, 0, elem.steps))
        case (Nil, 'L') => Seq(Block(0, 0, elem, 0, -elem.steps))
        case (Nil, 'U') => Seq(Block(0, 0, elem, elem.steps, 0))
        case (Nil, 'D') => Seq(Block(0, 0, elem, -elem.steps, 0))
        case (next, 'R') => next :+ Block(next.last.endRow, next.last.endCol, elem, next.last.endRow, next.last.endCol + elem.steps)
        case (next, 'L') => next :+ Block(next.last.endRow, next.last.endCol, elem, next.last.endRow, next.last.endCol - elem.steps)
        case (next, 'U') => next :+ Block(next.last.endRow, next.last.endCol, elem, next.last.endRow - elem.steps, next.last.endCol)
        case (next, 'D') => next :+ Block(next.last.endRow, next.last.endCol, elem, next.last.endRow + elem.steps, next.last.endCol)
      }
    })
    val colMin = blocks.map(_.col).min
    val colOffset = if (colMin < 0) abs(colMin) else 0
    val rowMin = blocks.map(_.row).min
    val rowOffset = if (rowMin < 0) abs(rowMin) else 0
    val adjustedBlocks = blocks.map(b => b.copy(row = b.row + rowOffset, col = b.col + colOffset))
    adjustedBlocks.map(b => {
      b.digPlan.direction match
        case 'R' => b.digPlan.steps
        case 'D' => b.digPlan.steps * (b.col + 1)
        case 'U' => b.digPlan.steps * -b.col
        case _ => 0

    }).sum + 1
  }

  def problem1(filePath: String): Long = {
    val input = readFile(filePath)
    buildLavaPool(input)
  }

  private def getDirection: DigPlan => Char = _.color.stripSuffix(")").last match
    case '0' => 'R'
    case '1' => 'D'
    case '2' => 'L'
    case '3' => 'U'

  private def getSteps(d: DigPlan) = java.lang.Long.parseLong(d.color.substring(2, 7), 16)


  def problem2(filePath: String): Long = {
    val input = readFile(filePath)
    val updatedInput = input.map(d => d.copy(direction = getDirection(d), steps = getSteps(d)))
    buildLavaPool(updatedInput)
  }
}