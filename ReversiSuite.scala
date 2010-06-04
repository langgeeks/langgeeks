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
     val startBoard = board.startGame
     assertEquals(Square.Black, startBoard(3)(3))
     assertEquals(Square.White, startBoard(3)(4))
     assertEquals(Square.White, startBoard(4)(3))
     assertEquals(Square.Black, startBoard(4)(4))
  }

  @Test
  def oneLineOfDotsIsEightEmptySquares() {
      val row = board.createRow("........")
      val emptyRow = Array(Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty, Square.Empty)
      assertEquals(emptyRow.size, row.size)
      for (square <- row)
           assertEquals(Square.Empty, square)
  }

  @Test
  def oneLineWithAllTypesOfSquares() {
      val row = board.createRow(".WBO")
      assertEquals(Square.Empty, row(0))
      assertEquals(Square.White, row(1))
      assertEquals(Square.Black, row(2))
      assertEquals(Square.Possible, row(3))
  }

  @Test
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
      val row = board.createRow("...WB...")
      val expected = board.createRow("..OWB...")
      val evaluated = board.evaluate(row)
      assert(expected.toList == evaluated.toList)
  }
}

