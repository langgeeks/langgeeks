import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import Square._

class Board {
  import Board._
  var squares: Array[Array[Square.Value]] = _
  var player = Black  

  def startGame() = {
    parseBoard("""
                    ........
                    ........
                    ........
                    ...BW...
                    ...WB...
                    ........
                    ........
                    ........
                    B
    """)
  }


  def parseBoard(board: String) = {
    squares = stripLines(board).map{createRow(_)}
    if (squares.size > 1) {
      player = squares.last.last
      squares = squares.dropRight(1)
    }
    squares
  }

  def opponent() = {
    if (player == Black)
      White
    else
      Black
  }

  def evaluate() {
    squares = evaluateRowsTopDown(squares)
    squares = evaluateColumnsLeftToRight(squares)
    squares = evaluateDiagonals(squares)
  }

  def evaluateRowsTopDown(board: Array[Array[Square.Value]]) = {
    board.map { row =>
      evaluateRow(evaluateRow(row).reverse).reverse
    }
  }

  def evaluateColumnsLeftToRight(board: Array[Array[Square.Value]]) = {
    evaluateRowsTopDown(board.transpose).transpose
  }

  def evaluateDiagonals(board: Array[Array[Square.Value]]) = {
    val leftToRight = evaluateDiagonally(board)
    val turned = clockwise(leftToRight)
    val upAndDown = evaluateDiagonally(turned)
    counterClockwise(upAndDown)
  }

  def evaluateDiagonally(board: Array[Array[Square.Value]]) = {
    val d = diagonals(board).map { row =>
      evaluateRow(evaluateRow(row).reverse).reverse
    }
    backToMatrix(d)
  }

  def backToMatrix(diagonals: Array[Array[Square.Value]], matrix:ArrayBuffer[Array[Square.Value]] = new ArrayBuffer()):Array[Array[Square.Value]] = {
    if(diagonals.isEmpty)
      return matrix.toArray
    val r = rowFrom(diagonals)
    matrix += r
    val stripped = stripAwayRowFrom(diagonals)
    backToMatrix(stripped, matrix)
  }

  def rowFrom(diagonals: Array[Array[Square.Value]]) = {
    for(i <- 0 until squares.size) yield diagonals(i).last
  }.toArray

  def stripAwayRowFrom(diagonals: Array[Array[Square.Value]]) = {
    val r = new ArrayBuffer[Array[Square.Value]]()
    for(i <- 0 until diagonals.size)  {
      if(i >= squares.size ) r += diagonals(i)
      else if(diagonals(i).init.size != 0) r += diagonals(i).init
    }
    r.toArray
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
    if (row.head == Empty || row.head == Possible)
      return false
    if (row.head == player && atTheStart)
      return false
    if (row.head == player && !atTheStart)
      return true
    return possibleMove(row.tail, false)
  }

  def diagonals(squares: Array[Array[Square.Value]]) = {
    val size = squares.size - 1
    val reply: ListBuffer[Array[Square.Value]] = ListBuffer()
    
    for (i <- 0 to size) {    
      val row: ListBuffer[Square.Value] = ListBuffer()
      var x = i                
      var y = 0                
      while(x >= 0) {          
        row += squares(x)(y)
        x -= 1                   
        y += 1                   
      }
      reply += row.toArray
    }
    for (i <- 0 to size - 1) {
      val row: ListBuffer[Square.Value] = ListBuffer()
      for (x <- size until i by -1) {
        var y = size - x + i + 1
        row += squares(x)(y)
      }
      reply += row.toArray
    }
    reply.toArray
  }

  def clockwise(squares: Array[Array[Square.Value]]) = {
    for (i <- 0 until squares.size) yield {
      for(j <- squares.size -1 to 0 by -1) yield squares(j)(i)
    }.toArray
  }.toArray

  def counterClockwise(squares: Array[Array[Square.Value]]) = {
    for (i <- squares.size - 1 to 0 by -1) yield {
      for(j <- 0 until squares.size) yield squares(j)(i)
    }.toArray
  }.toArray

  override def toString() = {
    Board.toString(squares, player)
  }
  
}

object Board {

  def strip(board: String) = {
    stripLines(board).mkString("\n")
  }

  def stripLines(board: String) = {
    board.split("\n").map{_.trim}.filter{_.size != 0}
  }

  def createRow(row: String) = {
    row.map { Square(_) }.toArray
  }

  def toString(squares: Array[Array[Square.Value]], player: Square.Value = Square.White) = {
    squares.map{ _.mkString }.mkString("", "\n", "\n") + player
  }
}

