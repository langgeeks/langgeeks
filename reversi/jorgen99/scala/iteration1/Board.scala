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

  def evaluate(row: Array[Square.Value]): Array[Square.Value] = {
    val firstNonempty = indexOfFirstNonEmptySquare(row)
    if(firstNonempty == -1) return row
    if(firstNonempty == 7) return row
    if(row(firstNonempty) == Black) return row
    if(anyOfColorFromFirstNonEmpy(row, Black))
      return markPossibleMoveForColor(row, Black)
    throw new RuntimeException
  }

  def anyOfColorFromFirstNonEmpy(row: Array[Square.Value], color: Square.Value): Boolean = {
    row.drop(indexOfFirstNonEmptySquare(row)).exists(_ == color)
  }
  
  def markPossibleMoveForColor(row: Array[Square.Value], color: Square.Value): Array[Square.Value] = {
    row(indexOfFirstNonEmptySquare(row) - 1) = Possible
    row
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
