package problem

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs
import scala.util.Using


final case class DigPlan(direction: Char, steps: Int, color: String)

final case class Block(row: Int, col: Int, color: String)

object LavaductLagoon {

  def readFile(filePath: String) = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(_.split(" ") match
          case Array(direction, steps, color) => DigPlan(direction.head, steps.toInt, color)
        ).toSeq
    }.get
  }

  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
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
    val pool = Array.fill(height)(Array.fill(width)('.'))
    blocks.foreach(k => pool(k.row + heightOffset)(k.col + widthOffset) = '#')
    pool.foreach(c => {
      c.foreach(print)
      println
    })
    println
    val updatedPool = ArrayBuffer.from(pool.map(c => ArrayBuffer.from(c)))
    pool.indices.foreach(r => {
      var inside = false
      var cutoff = 0
      pool(r).indices.foreach(c => {
        if (c == 233 && r == 5) {
          println()
        }
        val nextInsideStart = pool(r).indexOf('#', cutoff)
        if (!inside && nextInsideStart == c) {
          inside = true
        }
        if (inside) {
          updatedPool(r)(c) = '#'
          if (c + 1 < pool(r).length && pool(r)(c + 1) == '.') {

          }
        }


      })
    })
    updatedPool.foreach(c => {
      c.foreach(print)
      println
    })
    pool.flatten.count(_ == '#')
  }

  def problem2(filePath: String): Iterator[String] = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
    }.get
  }
}