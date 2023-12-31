package extensions

import model.graph.Node

object SeqExtensions {

  extension (listOfList: Seq[Seq[_]]) {

    def toGraphNodes: Seq[Node] = {
      listOfList.indices.flatMap(row => listOfList(row).indices.map(col => Node(row, col)))
    }


  }
  extension (listOfLong: Seq[Long]) {
    def lcm: Long = listOfLong.foldLeft(1: Long) { (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs }

  }
}