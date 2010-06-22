import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Ignore
import org.junit.Before
import org.junit.Assert._

import Square._

class ReversiSuite extends AssertionsForJUnit {
  var board: Board = _

  @Before
  def setup() {
    board = new Board
    board.startGame
  }

  @Test
  def startBoardIsSetupCorrectly() {
    assertEquals(Black, board.squares(3)(3))
    assertEquals(White, board.squares(3)(4))
    assertEquals(White, board.squares(4)(3))
    assertEquals(Black, board.squares(4)(4))
  }

  @Test
  def oneLineOfDotsIsEightEmptySquares() {
    val row = board.createRow("........")
    val emptyRow = Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)
    assertEquals(emptyRow.size, row.size)
    for (square <- row)
      assertEquals(Empty, square)
  }

  @Test
  def oneLineWithAllTypesOfSquares() {
    val row = board.createRow(".WBO")
    assertEquals(Empty, row(0))
    assertEquals(White, row(1))
    assertEquals(Black, row(2))
    assertEquals(Possible, row(3))
  }

  @Test
  def indexOfFirstNonEmptySquareIsThree() {
    val row = board.createRow("..WWB...")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(2, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsFour() {
    val row = board.createRow("...WWB..")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(3, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsMinusOne() {
    val row = board.createRow("........")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(-1, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsOne() {
    val row = board.createRow("W.......")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(0, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsEight() {
    val row = board.createRow(".......W")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(7, index)
  }

  @Test
  def onlyEmptyRowsHaveNoPossibleMoves() {
    val expected = board.createRow("........")
    board.evaluate()
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def noPossibleMovesForBlackWithOnlyOnePieceOnTheEighthSquare() {
    val row = board.createRow(".......W")
    board.squares(0) = row
    val expected = board.createRow(".......W")
    board.evaluate()
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def noPossibleMovesForBlackWhenFirstNonEmptySquareIsBlack() {
    val row = board.createRow("..B....")
    board.squares(0) = row
    val expected = board.createRow("..B....")
    board.evaluate()
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
    val row = board.createRow("...WB...")
    board.squares(0) = row
    val expected = board.createRow("..OWB...")
    board.evaluate()
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def anyOfColorFromIndexReturnsTrue() {
    val row = board.createRow("...WB...")
    board.squares(0) = row
    assertTrue(board.anyOfColorFromFirstNonEmpy(0, Black))
  }

  @Test
  def anyOfColorFromIndexReturnsFalse() {
    val row = board.createRow("...WWW..")
    board.squares(0) = row
    assertFalse(board.anyOfColorFromFirstNonEmpy(0, Black))
  }

  @Test
  def markPossibleMoveForColor() {
    val row = board.createRow("...WB...")
    board.squares(0) = row
    val expected = board.createRow("..OWB...")
    board.markPossibleMoveForColor(0, Black)
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRight() {
    val row = board.createRow("...WBW..")
    board.squares(0) = row
    val expected = board.createRow("..OWBWO.")
    board.evaluate()
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

}
