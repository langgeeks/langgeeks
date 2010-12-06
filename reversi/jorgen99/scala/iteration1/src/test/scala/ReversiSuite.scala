import org.scalatest.junit.JUnitSuite
// import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Ignore
import org.junit.Before
import org.junit.Assert._

import Square._

class ReversiSuite extends JUnitSuite {
// class ReversiSuite extends AssertionsForJUnit {
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
    val squares = board.parseBoard("...WBW..\nB")
    val evaluated = board.evaluateRowsTopDown(squares)
    val expected = board.createRow("..OWBWO.")
    assertEquals(expected.mkString, evaluated(0).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRightWithMoreThanOneBlackInbetween() {
    val squares = board.parseBoard(".BBBWB..\n" + "W")
    val evaluated = board.evaluateRowsTopDown(squares)
    val expected = board.createRow("OBBBWBO.")
    assertEquals(expected.mkString, evaluated(0).mkString)
  }

  @Test
  def possibleMovesShouldBeTreatedLikeAnEmptySquare() {
    val row = board.createRow("..OWB...")
    val expected = board.createRow("..OWB...")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
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

  def barString(board: Array[Array[Square.Value]]) = {
    board.map { _.mkString }.mkString("|")
  }


  @Test
  def evaluateBoardLeftToRight() {
    val game = (
      "      ....W..W       \n" +
      "      B...B..B       \n" +
      "      W......B       \n" +
      "      ........       \n" +
      "      W              \n");


    val theBoard = board.parseBoard(game)
    val squares = board.evaluateColumnsLeftToRight(theBoard)
    val expected = "O...W..W\nB...B..B\nW...O..B\n.......O\nW"
    assertEquals(expected, board.toString(squares))
  }

  @Test
  def evaluateBoardTopDownAndLeftToRight() {
    val game = (
      "      ...BW..W       \n" +
      "      B..WB..B       \n" +
      "      WBB....B       \n" +
      "      .....WB.       \n" +
      "      W              \n");

    val theBoard = board.parseBoard(game)
    var squares = board.evaluateRowsTopDown(theBoard)
    squares = board.evaluateColumnsLeftToRight(squares)

    val expected = "O.OBW..W\nB..WBO.B\nWBBOO..B\n.....WBO\nW"
    assertEquals(expected, board.toString(squares))
  }

  @Test
  def shouldEvaluateStartPosition() {
    val game = (
      "     ........     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     ...BW...     \n" +
      "     ...WB...     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     ........     \n" +
      "     B              "
    )
    board.parseBoard(game)
    board.evaluate

    val expected = "........\n........\n....O...\n...BWO..\n..OWB...\n...O....\n........\n........\nB"
    assertEquals(expected, board.toString)
  }

  @Test
  def diagonalsShouldReturTheCorrectSize() {
    val game = (
      "      WWWW       \n" +
      "      WWWW       \n" +
      "      WWWW       \n" +
      "      WWWW       \n" +
      "      W              \n");

    val theBoard = board.parseBoard(game)
    assertEquals(4, theBoard.size)
    val diagonals = board.diagonals(theBoard)
    assertEquals(7, diagonals.size)
  }

  @Test
  def diagonalsShouldReturnDiagonals() {
    val game = (
      "      WBBW       \n" +
      "      WBBW       \n" +
      "      WBBW       \n" +
      "      WBBW       \n" +
      "      W              \n");

    val theBoard = board.parseBoard(game)
    assertEquals(4, theBoard.size)
    val diagonals = board.diagonals(theBoard)
    assertEquals(7, diagonals.size)
    val expected = "W\nWB\nWBB\nWBBW\nBBW\nBW\nW\nW"
    assertEquals(expected, board.toString(diagonals))
  }

  @Test
  def shouldStripAwayFromRows() {
    val game = (
      "     ....     \n" +
      "     ..B.     \n" +
      "     .B..     \n" +
      "     W...     \n" +
      "     W          "
    )

    val squares = board.parseBoard(game)
    val biff = board.diagonals(squares)

    var stripped = board.stripAwayRowFrom(biff)
    var strippedString = stripped.map { _.mkString }.mkString("|")
    assertEquals(".|..|WBB|...|..|.", strippedString)

    stripped = board.stripAwayRowFrom(stripped)
    strippedString = stripped.map { _.mkString }.mkString("|")
    assertEquals(".|WB|..|..|.", strippedString)

    val nextRow = board.rowFrom(stripped)
    assertEquals(".B..", nextRow.mkString)

    stripped = board.stripAwayRowFrom(stripped)
    strippedString = stripped.map { _.mkString }.mkString("|")
    assertEquals("W|.|.|.", strippedString)

    stripped = board.stripAwayRowFrom(stripped)
    assertTrue(stripped.isEmpty)
  }

  @Test
  def shouldEvaluateDiagonal() {
    val game = (
      "     ....     \n" +
      "     ..B.     \n" +
      "     .B..     \n" +
      "     W...     \n" +
      "     W          "
    )
    val squares = board.parseBoard(game)
    assertEquals(4, board.squares.size)
    val b = board.evaluateDiagonally(squares)
    val expected = "...O|..B.|.B..|W..."
    assertEquals(expected, b.map {_.mkString}.mkString("|"))
  }

  @Test
  def shouldRotateCounterClockwise() {
    val game = (
      "     ....     \n" +
      "     ..B.     \n" +
      "     .B..     \n" +
      "     W...     \n" +
      "     W          "
    )
    val b = board.parseBoard(game)
    val rotated = board.counterClockwise(b)
    val expected = "....|.B..|..B.|...W"
    assertEquals(expected, rotated.map {_.mkString}.mkString("|"))
  }

  @Test
  def shouldRotateClockwise() {
    val game = (
      "     ....     \n" +
      "     ..B.     \n" +
      "     .B..     \n" +
      "     W...     \n" +
      "     W          "
    )
    val b = board.parseBoard(game)
    val rotated = board.clockwise(b)
    val expected = "W...|.B..|..B.|...."
    assertEquals(expected, rotated.map {_.mkString}.mkString("|"))
  }

  @Test
  def shouldEvaluateTwoDiagonals() {
    val game = (
      "     ...W     \n" +
      "     .BB.     \n" +
      "     .BB.     \n" +
      "     ...W     \n" +
      "     W              "
    )
    val squares = board.parseBoard(game)
    val b = board.evaluateDiagonals(squares)
    val expected = "O..W|.BB.|.BB.|O..W"
    assertEquals(expected, b.map {_.mkString}.mkString("|"))
  }

  @Test
  def finalAcceptanceTestShouldEvaluateOk() {
    val game = (
      "     ..BB....     \n" +
      "     .WWWB...     \n" +
      "     .WWWBW..     \n" +
      "     ...BW...     \n" +
      "     ...WB...     \n" +
      "     ....W...     \n" +
      "     ..BWW...     \n" +
      "     ........     \n" +
      "     B              "
    )
    board.parseBoard(game)
    board.evaluate
 
// O.BB....
// OWWWB...
// OWWWBWO.
// O.OBWOO.
// ..OWB...
// ...OW...
// ..BWWO..
// ....O...
// B

    val expected = "O.BB....|OWWWB...|OWWWBWO.|O.OBWOO.|..OWB...|...OW...|..BWWO..|....O..."
    assertEquals(expected, barString(board.squares))
  }
}
