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

  // @Test
  // def indexOfFirstNonEmptySquareIsMinusOne() {
  //   val row = board.createRow("........")
  //   val index = board.indexOfFirstNonEmptySquare(row)
  //   assertEquals(-1, index)
  // }

  // @Test
  // def noPossibleMovesForBlackWithOnlyOnePieceOnTheEighthSquare() {
  //   board.parseBoard(".......W")
  //   board.evaluate()
  //   val expected = board.createRow(".......W")
  //   assertEquals(expected.mkString, board.squares(0).mkString)
  // }

  // @Test
  // def noPossibleMovesForBlackWhenFirstNonEmptySquareIsBlack() {
  //   board.parseBoard("..B....")
  //   board.evaluate()
  //   val expected = board.createRow("..B....")
  //   assertEquals(expected.mkString, board.squares(0).mkString)
  // }

  // @Test
  // def blackHasOnePossibleMoveToTheLeftOnOneLine() {
  //   board.parseBoard("...WB...")
  //   board.evaluate()
  //   val expected = board.createRow("..OWB...")
  //   assertEquals(expected.mkString, board.squares(0).mkString)
  // }

  // @Test
  // @Ignore
  // def blackHavePossibleMovesToTheLeftAndToTheRight() {
  //   board.parseBoard("...WBW..")
  //   board.evaluate()
  //   val expected = board.createRow("..OWBWO.")
  //   assertEquals(expected.mkString, board.squares(0).mkString)
  // }

}
