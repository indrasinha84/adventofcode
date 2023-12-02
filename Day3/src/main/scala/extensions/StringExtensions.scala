package extensions

object StringExtensions {

  extension (str: String) {
    def toCubesMap: (Int, Seq[Map[String, Int]]) = {
      val colonSplitted = str.split(":").toSeq
      val (gameNumber, others) = (colonSplitted.head.split(" ").last.toInt, colonSplitted.last)
      (gameNumber, others.trim.split(";").map(_.split(",").map(
        cubesStr => {
          val splittedCube = cubesStr.trim.split(" ")
          val color = splittedCube.last
          val count = splittedCube.head
          color -> count.toInt
        }
      ).toMap).toSeq)
    }
  }
}