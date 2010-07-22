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

  def opponent() = {
    if (player == Black)
      White
    else
      Black
  }

  def evaluate() {
    squares = squares.map { row: Array[Square.Value] =>
      evaluateRow(evaluateRow(row).reverse).reverse
    }
  }

  def evaluateRow(row: Array[Square.Value]): Array[Square.Value] = {
    if (row.isEmpty)
      return row
    if (row.head == Empty && possibleMove(row.tail))
      return Possible +: evaluateRow(row.tail)
    row.head +: evaluateRow(row.tail)
  }

  def possibleMove(row: Array[Square.Value]): Boolean = {
    possibleMove(row, true)
  }

  def possibleMove(row: Array[Square.Value], atTheStart: Boolean): Boolean = {
    if (row.isEmpty)
      return false
    if (row.head == Empty)
      return false
    if (row.head == player && atTheStart)
      return false
    if (row.head == player && !atTheStart)
      return true
    return possibleMove(row.tail, false)
  }

  override def toString() = {
    val str = squares.map {
      _.mkString + "\n"
    }.mkString
    if (player != null)
      str + player
    else
      str
  }
  
}

