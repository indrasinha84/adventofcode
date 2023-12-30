package extensions

object StringExtensions {


  extension (str: String) {
    def lowestAdventDigit(digits: Map[String, Int]): Int = {
      digits.foldLeft((str, 0))((remainig, d) => {
        val index = remainig._1.indexOf(d._1)
        if (index > -1) (remainig._1.substring(0, index + d._1.length), d._2) else remainig
      })._2
    }

    def highestAdventDigit(digits: Map[String, Int]): Int = {
      digits.foldLeft((str, 0))((remainig, d) => {
        val index = remainig._1.lastIndexOf(d._1)
        if (index > -1) (remainig._1.substring(index), d._2) else remainig
      })._2
    }
  }
}