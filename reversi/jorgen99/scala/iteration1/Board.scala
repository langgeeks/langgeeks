import scala.collection.mutable.ArrayBuffer
import Square._

class Board {
  var squares:Array[Array[Square.Value]] = _

  def startGame() = {
    squares = Array(
      createRow("........"),
      createRow("........"),
      createRow("........"),
      createRow("...BW..."),
      createRow("...WB..."),
      createRow("........"),
      createRow("........"),
      createRow("........"))
  }

  def createRow(row: String): Array[Square.Value] = {
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
    if(anyOfColorFromFirstNonEmpy(row, Black)) {
      markPossibleMoveForColor(rowIndex, Black)
      return 
    }
    throw new RuntimeException
  }

  def anyOfColorFromFirstNonEmpy(row: Array[Square.Value], color: Square.Value): Boolean = {
    row.drop(indexOfFirstNonEmptySquare(row)).exists(_ == color)
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
