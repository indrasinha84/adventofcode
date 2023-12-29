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

    def lpad(c: Char, n: Int): String =
      (c.toString * (n - str.length)) + str

    def splitOnHashOrQuestionMark: Option[(String, String)] = {
      val indexOfHash = str.indexOf('#')
      val indexOfQuestionMark = str.indexOf('?')
      (indexOfHash, indexOfQuestionMark) match
        case (-1, -1) => None
        case (-1, q) => Some(str.splitAt(q))
        case (h, -1) => Some(str.splitAt(h))
        case (h, q) => Some(str.splitAt(Math.min(h, q)))
    }


    def splitIntoTwoOnDot: Option[(String, String)] = {
        str.indexOf('.') match
        case -1 => None
        case d => Some {
          str.splitAt(d)
        }
    }

    def removeFirstDots(): String = {
      var newStr = str.stripMargin('.')
      while (newStr != newStr.stripMargin('.')) {
        newStr = newStr.stripMargin('.')
      }
      newStr
    }
      }


}