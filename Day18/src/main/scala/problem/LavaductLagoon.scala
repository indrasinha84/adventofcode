package problem

import scala.collection.mutable
import scala.io.Source
import scala.math.abs
import scala.util.Using


final case class DigPlan(direction: Char, steps: Int, color: String)

final case class Block(row: Int, col: Int, color: String)

object LavaductLagoon {

  private def readFile(filePath: String) = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(_.split(" ") match
          case Array(direction, steps, color) => DigPlan(direction.head, steps.toInt, color)
        ).toSeq
    }.get
  }

  def buildLavaPool(input: Seq[DigPlan]) = {
    val start = Block(0, 0, "")
    val blocks = input.foldLeft(Seq(start))((sum, elem) => {
      sum ++ (elem.direction match {
        case 'R' => Seq.range(1, elem.steps + 1).map(s => Block(sum.last.row, sum.last.col + s, elem.color))
        case 'L' => Seq.range(1, elem.steps + 1).map(s => Block(sum.last.row, sum.last.col - s, elem.color))
        case 'U' => Seq.range(1, elem.steps + 1).map(s => Block(sum.last.row - s, sum.last.col, elem.color))
        case 'D' => Seq.range(1, elem.steps + 1).map(s => Block(sum.last.row + s, sum.last.col, elem.color))
        case _ => Seq.empty[Block]
      })
    })
    val widthOffset = abs(blocks.map(_.col).min)
    val width = blocks.map(_.col).max + widthOffset + 1
    val heightOffset = abs(blocks.map(_.row).min)
    val height = blocks.map(_.row).max + heightOffset + 1
    val pool = mutable.Seq.fill(height)(mutable.Seq.fill(width)('.'))
    blocks.foreach(k => pool(k.row + heightOffset)(k.col + widthOffset) = '#')
    val startingPoint = pool.indices.flatMap(r => pool(r).indices.map(c => (r, c))).find({ case (r, c) =>
      pool(r)(c) == '#' && pool(r)(c + 1) == '#' && pool(r + 1)(c) == '#' && pool(r + 1)(c + 1) == '.'
    }).map(p => (p._1 + 1, p._2 + 1)).getOrElse((-1, -1))

    val stack = mutable.Stack(startingPoint)
    while (stack.nonEmpty) {
      val (r, c) = stack.pop()
      Seq((r - 1, c - 1), (r - 1, c), (r - 1, c + 1), (r, c - 1), (r, c + 1), (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)).foreach({ case (r, c) =>
        pool(r)(c) match
          case '.' =>
            stack.push((r, c))
            pool(r)(c) = '#'
          case _ =>
      })
    }
    pool.flatten.count(_ == '#')
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    buildLavaPool(input)
  }

  private def getDirection: DigPlan => Char = _.color.stripSuffix(")").last match
      case '0' => 'R'
      case '1' => 'D'
      case '2' => 'L'
      case '3' => 'U'

  private def getSteps(d: DigPlan) = Integer.parseInt(d.color.substring(2, 7), 16)


  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    val updatedInput = input.map(d => d.copy(direction = getDirection(d), steps = getSteps(d)))
    buildLavaPool(updatedInput)
  }
}