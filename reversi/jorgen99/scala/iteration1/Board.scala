import Square._

class Board {
  var squares:Array[Array[Square.Value]] = _

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
  }

  def stripLines(board: String) = {
    board.split("\n").map{_.trim}.filter{_.size != 0}
  }

  def createRow(row: String) = {
    row.map { Square.parse(_) }.toArray
  }

  def evaluate() {
    for (i <- 0 until squares.size)
      evaluate(i)
  }

  def evaluate(rowIndex: Int) {
    val row = squares(rowIndex)
    val firstNonempty = indexOfFirstNonEmptySquare(row)
    if(firstNonempty == -1) return row
    if(firstNonempty == 7) return row
    if(row(firstNonempty) == Black) return row
    if(anyOfColorFromFirstNonEmpy(rowIndex, Black)) {
      markPossibleMoveForColor(rowIndex, Black)
      return 
    }
    throw new RuntimeException
  }

  def anyOfColorFromFirstNonEmpy(rowIndex: Int, color: Square.Value) = {
    squares(rowIndex).drop(indexOfFirstNonEmptySquare(rowIndex)).exists(_ == color)
  }
  
  def markPossibleMoveForColor(rowIndex: Int, color: Square.Value) = {
    val nonEmpty = indexOfFirstNonEmptySquare(rowIndex)
    squares(rowIndex)(nonEmpty - 1) = Possible
  }

  def indexOfFirstNonEmptySquare(rowIndex: Int): Int = {
    indexOfFirstNonEmptySquare(squares(rowIndex), 0)
  }

  def indexOfFirstNonEmptySquare(row: Array[Square.Value]): Int = {
    indexOfFirstNonEmptySquare(row, 0)
  }

  private def indexOfFirstNonEmptySquare(row: Array[Square.Value], startIndex:Int): Int = {
    if (row.size == startIndex)
      return -1
    if (row(startIndex) != Empty)
      return startIndex
    else
      return indexOfFirstNonEmptySquare(row, startIndex + 1)
  }

}
