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
  def anEmptyRowMakesItANonPossibleMove() {
    val row = board.createRow("..")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def blackAsFirstTokenMakesItANonPossibleMove() {
    val row = board.createRow("B...")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def aSingleWhiteMakesItANonPossibleMove() {
    val row = board.createRow("W...")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def aWhiteFollowedByABlackIsAPossibleMove() {
    val row = board.createRow("WB..")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def anyNumberOfWhiteFollowedByABlackIsAPossibleMove() {
    val row = board.createRow("WWWB.")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def aWhiteFollowedByManyBlackIsAPossibleMove() {
    val row = board.createRow("WBBBB.")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
    val row = board.createRow("...WB...")
    val expected = board.createRow("..OWB...")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def testingRowWithMoreThenOnePossibleMove() {
    val row = board.createRow(".WB..WB.")
    val expected = board.createRow("OWB.OWB.")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRight() {
    val row = board.parseBoard("...WBW..\n" + "B")
    board.evaluate
    val expected = board.createRow("..OWBWO.")
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRightWithMoreThanOneBlackInbetween() {
    val row = board.parseBoard(".BBBWB..\n" + "W")
    board.evaluate
    val expected = board.createRow("OBBBWBO.")
    assertEquals(expected.mkString, board.squares(0).mkString)
  }

  @Test
  def testBoardToString() {
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
    val expected = "W.......\n.WB.W...\n..BB.B..\n..BWBW..\n..BBW...\n.BBBWB..\n....W.B.\n........\nW"
    assertEquals(expected, board.toString)
  }

  @Test
  def evaluateBoardTopDown() {
    val game = (
      "      ..WB....       \n" +
      "      .....BW.       \n" +
      "      WBB.....       \n" +
      "      W              \n");

    board.parseBoard(game)
    board.evaluate
    val expected = "..WBO...\n....OBW.\nWBBO....\nW"
    assertEquals(expected, board.toString)

  }

}
