package problem

import extensions.StringExtensions.*

import scala.io.Source
import scala.util.Using

object CubeConundrum {
  def problem1(filePath: String): Int = {
    val limits = Map("red" -> 12, "green" -> 13, "blue" -> 14)
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(_.toCubesMap)
        .toMap
        .view
        .mapValues(_.flatten)
        .filterNot(_._2.exists(ballCount => ballCount._2 > limits(ballCount._1)))
        .keys
        .sum
    }.get
  }

  def problem2(filePath: String): Int = {
    Using(Source.fromFile(filePath)) { file =>
      file
        .getLines()
        .map(_.toCubesMap)
        .toMap
        .view
        .mapValues(_.flatten.groupBy(_._1).map(_._2.map(_._2).max).product)
        .values
        .sum
    }.get
  }
}