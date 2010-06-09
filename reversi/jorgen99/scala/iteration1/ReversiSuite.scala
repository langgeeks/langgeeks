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
    board = new Board()
  }

  @Test
  def startBoardIsSetupCorrectly() {
    val startBoard = board.startGame
    assertEquals(Black, startBoard(3)(3))
    assertEquals(White, startBoard(3)(4))
    assertEquals(White, startBoard(4)(3))
    assertEquals(Black, startBoard(4)(4))
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
    assertEquals(3, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsFour() {
    val row = board.createRow("...WWB..")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(4, index)
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
    assertEquals(1, index)
  }

  @Test
  def indexOfFirstNonEmptySquareIsEight() {
    val row = board.createRow(".......W")
    val index = board.indexOfFirstNonEmptySquare(row)
    assertEquals(8, index)
  }

  @Test
  def onlyEmptyRowsHaveNoPossibleMoves() {
    val row = board.createRow("........")
    val expected = board.createRow("........")
    val evaluated = board.evaluate(row)
    assert(expected.toList == evaluated.toList)
  }

  @Test
  def noPossibleMovesForBlackWithOnlyOnePieceOnTheEighthSquare() {
    val row = board.createRow(".......W")
    val expected = board.createRow(".......W")
    val evaluated = board.evaluate(row)
    assert(expected.toList == evaluated.toList)
  }

  @Test
  def noPossibleMovesForBlackWhenFirstSquareIsBlack() {
    val row = board.createRow("..B....")
    val expected = board.createRow("..B....")
    val evaluated = board.evaluate(row)
    assert(expected.toList == evaluated.toList)
  }

  @Test
  @Ignore
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
    val row = board.createRow("...WB...")
    val expected = board.createRow("..OWB...")
    val evaluated = board.evaluate(row)
    assert(expected.toList == evaluated.toList)
  }


}
