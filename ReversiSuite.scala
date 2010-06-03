import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Before
import org.junit.Assert._

class ReversiSuite extends AssertionsForJUnit {
 
  var board:Board = _

  @Before
  def setup() {
      board = new Board()
  }

  @Test
  def startBoardIsSetupCorrectly() {
     assertEquals(Square.Black, board.squares(3)(3))
     assertEquals(Square.White, board.squares(3)(4))
     assertEquals(Square.White, board.squares(4)(3))
     assertEquals(Square.Black, board.squares(4)(4))
  }

  @Test
  def oneLineOfDotsIsEightEmptySquares() {
      val row:Array[Square.Value] = board.createRow("........")
      val emptyRow = Array(Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty)
      assertEquals(emptyRow.size, row.size)
      for (square <- row)
           assertEquals(Square.Empty, square)
  }

}

