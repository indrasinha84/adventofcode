import model.graph.Node

package object utils {
  def isValidIndexF(rowLength: Int, colLength: Int)(p: (Int, Int)) = p._1 >= 0 && p._1 < rowLength && p._2 >= 0 && p._2 < colLength

  def isValidNodeF(rowLength: Int, colLength: Int)(p: Node) = p.row >= 0 && p.row < rowLength && p.col >= 0 && p.col < colLength

  def findNeighbouringNodes(position: Node, diagonal: Boolean = false): Seq[Node] = {
    (position.row, position.col) match
      case (r, c) => (Seq((r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)) ++
        (if (diagonal) Seq((r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)) else Seq.empty))
          .map({ case (r, c) => Node(r, c) })
  }

  def findNeighbours(position: (Int, Int), diagonal: Boolean = false): Seq[(Int, Int)] = {
    position match
      case (r, c) => Seq((r, c + 1), (r, c - 1), (r + 1, c), (r - 1, c)) ++
        (if (diagonal) Seq((r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)) else Seq.empty)
  }

  def findValidNeighboursF(isValidIndex: ((Int, Int)) => Boolean)(position: (Int, Int), diagonal: Boolean = false) = {
    findNeighbours(position, diagonal).filter(isValidIndex)
  }

  def lcm(list: Seq[Long]): Long = list.foldLeft(1: Long) { (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs }
  def lcmBigInt(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1)) { (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs }


}
