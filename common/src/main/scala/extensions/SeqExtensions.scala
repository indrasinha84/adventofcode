package extensions

import model.graph.Node

object SeqExtensions {

  extension (listOfList: Seq[Seq[_]]) {
    
    def toGraphNodes:  Seq[Node] = {
      listOfList.indices.flatMap(row => listOfList(row).indices.map(col => Node(row, col)))
    }

  }
}
