package problem

import scala.io.Source
import scala.util.Using

object CamelCards {


  val CardRankings = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2').reverse.zipWithIndex.toMap
  val CardRankingsWithJokers = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

  private def readFile(filePath: String) = Using(Source.fromFile(filePath)) { file =>
    file
      .getLines()
      .map(_.split(' ') match
        case Array(hand, bid) => (hand.toCharArray, bid.toInt)).toSeq
  }.get

  private def findHandQuality: Array[Char] => Int = _.groupBy(identity).view.mapValues(_.length).toMap match
    case g if g.exists(_._2 == 5) => 8
    case g if g.exists(_._2 == 4) => 7
    case g if g.exists(_._2 == 3) && g.exists(_._2 == 2) => 6
    case g if g.exists(_._2 == 3) => 5
    case g if g.count(_._2 == 2) == 2 => 4
    case g if g.exists(_._2 == 2) => 3
    case g if g.count(_._2 == 1) == 5 => 2
    case _ => 1


  private def findHandQualityWithJokers(hand: Array[Char]): Int = {
    val groups = hand.groupBy(identity).view.mapValues(_.length).toMap
    val jokerCount = groups.getOrElse('J', 0)
    val otherThanJoker = groups.filterNot(_._1 == 'J').values.toSet
    val highestOtherThanJoker = groups.filterNot(_._1 == 'J').values.maxOption.getOrElse(0)
    groups match
      case _ if highestOtherThanJoker + jokerCount == 5 => 8 //Five of a kind
      case _ if highestOtherThanJoker + jokerCount == 4 => 7 // Four of a kind
      case g if (g.exists(_._2 == 3) && g.exists(_._2 == 2)) || (jokerCount == 1 && otherThanJoker == Set(2, 2)) => 6 // Full house
      case g if g.exists(_._2 == 3) || (jokerCount == 2 && highestOtherThanJoker == 1) || (jokerCount == 1 && highestOtherThanJoker == 2) => 5 //Three of a kind
      case g if g.count(_._2 == 2) == 2 => 4 //Two pair
      case g if g.exists(_._2 == 2) || (jokerCount == 1 && highestOtherThanJoker == 1) => 3 // One pair
      case g if g.count(_._2 == 1) == 5 => 2 // High card
      case _ => 1
  }

  import extensions.StringExtensions.*

  private def findHandRankingCodes: Array[Char] => Int = _.map(c => CardRankings(c).toString.lpad('0', 2)).mkString.toInt
  private def findHandRankingCodesWithJokers: Array[Char] => Int = _.map(c => CardRankingsWithJokers(c).toString.lpad('0', 2)).mkString.toInt


  def problem1(filePath: String): Int = {
    val input = readFile(filePath)
    input.sortBy({ case (hand, _) => (findHandQuality(hand), findHandRankingCodes(hand)) }).zipWithIndex
      .map({ case ((_, bid), rnk) => bid * (rnk + 1) }).sum
  }

  def problem2(filePath: String): Int = {
    val input = readFile(filePath)
    input.sortBy({ case (hand, _) => (findHandQualityWithJokers(hand), findHandRankingCodesWithJokers(hand)) }).zipWithIndex
      .map({ case ((_, bid), rnk) => bid * (rnk + 1) }).sum
  }
}