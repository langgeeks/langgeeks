import scala.collection.mutable.ListBuffer

class Reversi(b: String) {
  import Reversi._

  var (board, player) = parse(b)

  val steps = Map (
      'E  -> ( 0,  1),
      'SE -> ( 1,  1),
      'S  -> ( 1,  0),
      'SW -> ( 1, -1),
      'W  -> ( 0, -1),
      'NW -> (-1, -1),
      'N  -> (-1,  0),
      'NE -> (-1,  1)
  )

  def opponent = {
    if (player == 'W')
      'B'
    else
      'W'
  }

  val allPositions = for (x <- 0 to 7; y <- 0 to 7) yield (x,y)

  def directions = steps.keySet

  def views(col: Int, row: Int) = viewsInAllDirections(col, row).values

  def viewsInAllDirections(col: Int, row: Int) = {
    directions.map( direction => (direction -> viewInDirection(col, row, direction)) ).toMap
  }

  def viewInDirection(row: Int, col: Int, direction: Symbol):String = {
    val (rowStep, colStep) = steps(direction)

    if(row < 0 || col < 0 || row > 7 || col > 7)
      ""
    else
      board(row)(col) + viewInDirection(row + rowStep, col + colStep, direction)
  }

  def legalInOneDirection(view: String) = {
    val legal = ("""^\.""" + opponent + "+" + player).r
    (legal findFirstIn view) match {
      case Some(s) => true
      case None => false
    }
  }

  def legalMove(col: Int, row: Int) = {
    views(col, row).exists { view => legalInOneDirection(view)  }
  }

  def legalMoves = {
    val clone = toListBuffer
    allPositions.foreach { case (x,y) =>
      if (legalMove(x, y)) clone(x)(y) = '0'
    }
    clone.append(ListBuffer(player))
    Reversi.toString(clone)
  }

  def toListBuffer = {
    val clone:ListBuffer[ListBuffer[Char]] = ListBuffer()
    for(i <- 0 to 7) {                                           
      var row: ListBuffer[Char] = ListBuffer()                       
      board(i).copyToBuffer(row)                                
      clone.append(row)
    }
    clone
  }

}


object Reversi {
  def apply(board: String) = new Reversi(board)

  def parse(board: String) = {
    val parsed = strip(board).split("\n").map { _.toCharArray.toList }.toList

    (parsed.init, parsed.last(0))
  }

  def strip(board:String) = {
    board
          .split("\n")
          .map { _ trim }
          .filter { _.size > 0 }
          .mkString("\n")
  }

  def toString(board: Seq[Seq[Char]]) = {
    board.map { _.mkString }.mkString("\n")
  }

}
