import Square._

class Board {
  var squares: Array[Array[Square.Value]] = _
  var player = Black  

  def startGame() = {
    parseBoard(
      "     ........     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     ...BW...     \n" +
      "     ...WB...     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     B              "
    )
  }


  def parseBoard(board: String) = {
    squares = stripLines(board).map{createRow(_)}
    if (squares.size > 1) {
      player = squares.last.last
      squares = squares.dropRight(1)
    }
    squares
  }

  def stripLines(board: String) = {
    board.split("\n").map{_.trim}.filter{_.size != 0}
  }

  def createRow(row: String) = {
    row.map { Square.parse(_) }.toArray
  }

  def evaluate() {
    squares.foreach {
      evaluateRow _
    }
  }

  def evaluateRow(row: Array[Square.Value]): Array[Square.Value] = {
    if (row.isEmpty)
      return row
    row.head +: evaluateRow(row.tail)
  }


}
