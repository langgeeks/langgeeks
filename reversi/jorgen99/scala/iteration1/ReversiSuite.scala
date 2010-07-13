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
  def playerIsBlackAtTheStartOfTheGame() {
    assertEquals(Black, board.player)
  }

  @Test
  def opponentIsWhiteAtTheStartOfTheGame() {
    assertEquals(White, board.opponent)
  }

  @Test
  def playerIsWhite() {
    val game = (
      "      W.......       \n" +
      "      .WB.W...       \n" +
      "      ..BB.B..       \n" +
      "      ..BWBW..       \n" +
      "      ..BBW...       \n" +
      "      .BBBWB..       \n" +
      "      ....W.B.       \n" +
      "             \n" +
      "      ........       \n" +
      "      W              \n");

    board.parseBoard(game)
    assertEquals(White, board.player)
  }

  @Test
  def evaluatingOneBlankRowShouldReturnOneBlankRow() {
    val row = board.createRow("........")
    val expected = board.createRow("........")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def noPossibleMovesForBlackWithOnlyOnePieceOnTheEighthSquare() {
    val row = board.createRow(".......W")
    val expected = board.createRow(".......W")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def noPossibleMovesIfRowIsFullOfTokens() {
    val row = board.createRow("WWWWWWWW")
    val expected = board.createRow("WWWWWWWW")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  @Ignore
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
    val row = board.createRow("...WB...")
    val expected = board.createRow("..OWB...")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def blackAsFirstTokenMakesItANonPossibleMove() {
    val row = board.createRow("B...")
    assertFalse(board.possibleMove(row))
  }

  // @Test
  // @Ignore
  // def blackHavePossibleMovesToTheLeftAndToTheRight() {
  //   board.parseBoard("...WBW..")
  //   board.evaluate()
  //   val expected = board.createRow("..OWBWO.")
  //   assertEquals(expected.mkString, board.squares(0).mkString)
  // }

}
