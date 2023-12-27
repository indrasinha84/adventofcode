

package object utils {
  def isValidIndexF(rowLength: Int, colLength: Int)(p: (Int, Int)) = p._1 >= 0 && p._1 < rowLength && p._2 >= 0 && p._2 < colLength

  def findNeighbours(position: (Int, Int), diagonal: Boolean = false): Seq[(Int, Int)] = {
    position match
      case (r, c) => Seq((r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)) ++
        (if (diagonal) Seq((r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)) else Seq.empty)
  }

  def findValidNeighboursF(isValidIndex: ((Int, Int)) => Boolean)(position: (Int, Int), diagonal: Boolean = false) = {
    findNeighbours(position, diagonal).filter(isValidIndex)
  }

}
