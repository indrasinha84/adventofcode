package extensions

object StringExtensions {

  val digits = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9)
  extension (str: String) {
    def lowestAdventDigit: Int = {
      digits.map(d => (str.indexOf(d._1), d._2)).filter(_._1 > -1).minByOption(a => a._1).map(_._2).getOrElse(0)
    }

    def highestAdventDigit: Int = {
      digits.map(d => (str.lastIndexOf(d._1), d._2)).filter(_._1 > -1).maxByOption(a => a._1).map(_._2).getOrElse(0)

    }
  }

}