package problem

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object IfYouGiveASeedAFertilizer {
  private final case class SeedConversionMap(destination: Long, sourceStart: Long, sourceEnd: Long, width: Long)

  private final case class SeedRange(start: Long, end: Long)

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    val lines = file
      .getLines().toSeq
    val seeds = lines.filter(_.contains("seeds:")).map(_.replace("seeds: ", "").split(' ').filterNot(_.isBlank).map(_.toLong).toSeq).head
    val mapStarts = lines.zipWithIndex.takeRight(lines.size - 2).filter(_._1.contains(":")).map(_._2 + 1)
    val mapEnds = lines.zipWithIndex.takeRight(lines.size - 2).filter(_._1.isBlank).map(_._2) :+ lines.size
    val conversionMaps = mapStarts.zip(mapEnds).map({ case (s, e) => TreeMap.from(lines.slice(s, e).map(_.split(' ') match
      case Array(destination, source, width) => source.toLong -> SeedConversionMap(destination.toLong, source.toLong, source.toLong + width.toLong - 1, width.toLong)))
    })
    (seeds, conversionMaps)
  }.get


  def problem1(filePath: String): Long = {
    val (seeds, conversionMaps) = readFile(filePath)
    val finalNumbers = seeds.map(s => {
      conversionMaps.foldLeft(s)((sum, c) => {
        c.filter({ case (k, _) => k <= sum }).lastOption.map({ case (_, c) =>
          if (sum <= (c.sourceStart + c.width)) sum + (c.destination - c.sourceStart) else sum
        }).getOrElse(sum)
      })
    })
    finalNumbers.min
  }

  def problem2(filePath: String): Long = {
    val (seeds, conversionMaps) = readFile(filePath)
    val (rangeStart, rangeEnd) = seeds.zipWithIndex.partition(_._2 % 2 == 0)
    val seedsRange = rangeStart.map(_._1).zip(rangeEnd.map(_._1)).map(r => SeedRange(r._1, r._1 + r._2 - 1))
    val conversionList = conversionMaps.map(_.values.toSeq.sortBy(c => (c.sourceStart, c.sourceEnd)))
    var rangesToConvert = mutable.ArrayBuffer.from(seedsRange.sortBy(c => (c.start, c.end)))
    conversionList.foreach(c => {
      // do conversion
      var conversionsRemaining = mutable.ArrayBuffer.from(c)
      val nextRange = mutable.ArrayBuffer.empty[SeedRange]
      while (rangesToConvert.nonEmpty && conversionsRemaining.nonEmpty) {
        val currentRange = rangesToConvert.head
        val currentConversion = conversionsRemaining.head
        if (currentRange.start < currentConversion.sourceStart) {
          if (currentRange.end < currentConversion.sourceStart) {
            nextRange += currentRange
            rangesToConvert = rangesToConvert.tail
          }
          else {
            nextRange += currentRange.copy(end = currentConversion.sourceStart - 1)
            rangesToConvert(0) = currentRange.copy(start = currentConversion.sourceStart)
          }
        }
        else if (currentRange.start > currentConversion.sourceEnd) {
          conversionsRemaining = conversionsRemaining.tail
        }
        else {
          if (currentRange.end <= currentConversion.sourceEnd) {
            nextRange += currentRange.copy(start = currentRange.start + (currentConversion.destination - currentConversion.sourceStart),
              end = currentRange.end + (currentConversion.destination - currentConversion.sourceStart))
            rangesToConvert = rangesToConvert.tail
          }
          else {
            nextRange += currentRange.copy(start = currentRange.start + (currentConversion.destination - currentConversion.sourceStart),
              end = currentConversion.sourceEnd + (currentConversion.destination - currentConversion.sourceStart))
            rangesToConvert(0) = currentRange.copy(start = currentConversion.sourceEnd + 1)
          }
        }
      }
      nextRange ++= rangesToConvert
      rangesToConvert = nextRange.sortBy(c => (c.start, c.end))
    })
    rangesToConvert.map(_.start).min
  }
}