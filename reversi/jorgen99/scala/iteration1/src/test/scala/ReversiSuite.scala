import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Ignore
import org.junit.Before
import org.junit.Assert._

import Square._
import Board._

class ReversiSuite extends JUnitSuite {
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
    val row = createRow("........")
    val emptyRow = Array(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)
    assertEquals(emptyRow.size, row.size)
    for (square <- row)
      assertEquals(Empty, square)
  }

  @Test
  def oneLineWithAllTypesOfSquares() {
    val row = createRow(".WBO")
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
    val game = ("""
            W.......   
            .WB.W...   
            ..BB.B..   
            ..BWBW..   
            ..BBW...   
            .BBBWB..   
            ....W.B.   
            ........   
            W          
      """)

    board.parseBoard(game)
    assertEquals(White, board.player)
  }

  @Test
  def evaluatingOneBlankRowShouldReturnOneBlankRow() {
    val row = createRow("........")
    val expected = createRow("........")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def noPossibleMovesForBlackWithOnlyOnePieceOnTheEighthSquare() {
    val row = createRow(".......W")
    val expected = createRow(".......W")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def noPossibleMovesIfRowIsFullOfTokens() {
    val row = createRow("WWWWWWWW")
    val expected = createRow("WWWWWWWW")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def anEmptyRowMakesItANonPossibleMove() {
    val row = createRow("..")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def blackAsFirstTokenMakesItANonPossibleMove() {
    val row = createRow("B...")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def aSingleWhiteMakesItANonPossibleMove() {
    val row = createRow("W...")
    assertFalse(row.mkString, board.possibleMove(row))
  }

  @Test
  def aWhiteFollowedByABlackIsAPossibleMove() {
    val row = createRow("WB..")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def anyNumberOfWhiteFollowedByABlackIsAPossibleMove() {
    val row = createRow("WWWB.")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def aWhiteFollowedByManyBlackIsAPossibleMove() {
    val row = createRow("WBBBB.")
    assertTrue(row.mkString, board.possibleMove(row))
  }

  @Test
  def blackHasOnePossibleMoveToTheLeftOnOneLine() {
    val row = createRow("...WB...")
    val expected = createRow("..OWB...")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def testingRowWithMoreThenOnePossibleMove() {
    val row = createRow(".WB..WB.")
    val expected = createRow("OWB.OWB.")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRight() {
    val squares = board.parseBoard("...WBW..\nB")
    val evaluated = board.evaluateRowsTopDown(squares)
    val expected = createRow("..OWBWO.")
    assertEquals(expected.mkString, evaluated(0).mkString)
  }

  @Test
  def blackHavePossibleMovesToTheLeftAndToTheRightWithMoreThanOneBlackInbetween() {
    val squares = board.parseBoard(".BBBWB..\n" + "W")
    val evaluated = board.evaluateRowsTopDown(squares)
    val expected = createRow("OBBBWBO.")
    assertEquals(expected.mkString, evaluated(0).mkString)
  }

  @Test
  def possibleMovesShouldBeTreatedLikeAnEmptySquare() {
    val row = createRow("..OWB...")
    val expected = createRow("..OWB...")
    assertEquals(expected.mkString, board.evaluateRow(row).mkString)
  }

  @Test
  def testBoardToString() {
    val game = ("""
            W.......   
            .WB.W...   
            ..BB.B..   
            ..BWBW..   
            ..BBW...   
            .BBBWB..   
            ....W.B.   
                   
            ........   
            W     
      """)

    board.parseBoard(game)
    val expected = "W.......\n.WB.W...\n..BB.B..\n..BWBW..\n..BBW...\n.BBBWB..\n....W.B.\n........\nW"
    assertEquals(expected, board.toString)
  }

  def barString(board: BoardMatrix) = {
    board.map { _.mkString }.mkString("|")
  }


  @Test
  def evaluateBoardLeftToRight() {
    val game = ("""
            ....W..W  
            B...B..B  
            W......B  
            ........  
            W        
    """)


    val theBoard = board.parseBoard(game)
    val squares = board.evaluateColumnsLeftToRight(theBoard)
    val expected = "O...W..W\nB...B..B\nW...O..B\n.......O\nW"
    assertEquals(expected, Board.toString(squares))
  }

  @Test
  def evaluateBoardTopDownAndLeftToRight() {
    val game = ("""
            ...BW..W    
            B..WB..B    
            WBB....B    
            .....WB.    
            W          
    """) 

    val theBoard = board.parseBoard(game)
    var squares = board.evaluateRowsTopDown(theBoard)
    squares = board.evaluateColumnsLeftToRight(squares)

    val expected = "O.OBW..W\nB..WBO.B\nWBBOO..B\n.....WBO\nW"
    assertEquals(expected, Board.toString(squares))
  }

  @Test
  def shouldEvaluateStartPosition() {
    val game = ("""
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
    board.parseBoard(game)
    board.evaluate

    val expected = "........\n........\n....O...\n...BWO..\n..OWB...\n...O....\n........\n........\nB"
    assertEquals(expected, board.toString)
  }

  @Test
  def diagonalsShouldReturTheCorrectSize() {
    val game = ("""
           WWWW    
           WWWW    
           WWWW    
           WWWW    
           W      
    """)

    val theBoard = board.parseBoard(game)
    assertEquals(4, theBoard.size)
    val diagonals = board.diagonals(theBoard)
    assertEquals(7, diagonals.size)
  }

  @Test
  def diagonalsShouldReturnDiagonals() {
    val game = ("""
           WBBW     
           WBBW     
           WBBW     
           WBBW     
           W        
    """)

    val theBoard = board.parseBoard(game)
    assertEquals(4, theBoard.size)
    val diagonals = board.diagonals(theBoard)
    assertEquals(7, diagonals.size)
    val expected = "W\nWB\nWBB\nWBBW\nBBW\nBW\nW\nW"
    assertEquals(expected, Board.toString(diagonals))
  }

  @Test
  def shouldStripAwayFromRows() {
    val game = ("""
           ....    
           ..B.    
           .B..    
           W...    
           W       
    """)

    val squares = board.parseBoard(game)
    val diagonals = board.diagonals(squares)

    var stripped = board.stripAwayRowFrom(diagonals)
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
    val game = ("""
                      ....  
                      ..B.  
                      .B..  
                      W...  
                      W     
    """)
    val squares = board.parseBoard(game)
    assertEquals(4, board.squares.size)
    val b = board.evaluateDiagonally(squares)
    val expected = """
                      ...O
                      ..B.
                      .B..
                      W...
                      W
    """
    assertEquals(strip(expected), Board.toString(b))
  }

  @Test
  def shouldRotateCounterClockwise() {
    val game = ("""
                      ....    
                      ..B.    
                      .B..    
                      W...    
                      W       
    """)
    val b = board.parseBoard(game)
    val rotated = board.counterClockwise(b)
    val expected = """
                      ....
                      .B..
                      ..B.
                      ...W
                      W
    """
    assertEquals(strip(expected), Board.toString(rotated))
  }

  @Test
  def shouldRotateClockwise() {
    val game = ("""
           ....    
           ..B.    
           .B..    
           W...    
           W       
    """)
    val b = board.parseBoard(game)
    val rotated = board.clockwise(b)
    val expected = """
                      W...
                      .B..
                      ..B.
                      ....
                      W
    """
    assertEquals(strip(expected), Board.toString(rotated))
  }

  @Test
  def shouldEvaluateTwoDiagonals() {
    val game = ("""
                      ...W     
                      .BB.     
                      .BB.     
                      ...W     
                      W           
    """)
    val squares = board.parseBoard(game)
    val b = board.evaluateDiagonals(squares)
    val expected = """
                      O..W
                      .BB.
                      .BB.
                      O..W
                      W
    """
    assertEquals(strip(expected), Board.toString(b))
  }

  @Test
  def finalAcceptanceTestShouldEvaluateOk() {
    val game = ("""
         ..BB....  
         .WWWB...  
         .WWWBW..  
         ...BW...  
         ...WB...  
         ....W...  
         ..BWW...  
         ........  
         B         
      """ )
    board.parseBoard(game)
    board.evaluate
 
    val expected = """
               O.BB....
               OWWWB...
               OWWWBWO.
               O.OBWOO.
               ..OWB...
               ...OW...
               ..BWWO..
               ....O...
               B
    """
    assertEquals(strip(expected), strip(board.toString))
  }
}
