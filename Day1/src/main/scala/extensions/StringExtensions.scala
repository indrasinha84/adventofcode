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
      digits.foldLeft((str, 0))((remainig, d) => {
        val index = remainig._1.indexOf(d._1)
        if (index > -1) (remainig._1.substring(0, index + d._1.length), d._2) else remainig
      })._2
    }

    def highestAdventDigit: Int = {
      digits.foldLeft((str, 0))((remainig, d) => {
        val index = remainig._1.lastIndexOf(d._1)
        if (index > -1) (remainig._1.substring(index), d._2) else remainig
      })._2
    }
  }

}