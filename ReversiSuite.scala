import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import org.junit.Assert._

class ReversiSuite extends AssertionsForJUnit {
 
 @Test
  def verifySetup() {
    assertEquals("1", 1.toString())
  }

  @Test
  def startBoardIsSetupCorrectly() {
     val board = new Board()
     assertEquals(Square.Black, board.squares(3)(3))
     assertEquals(Square.White, board.squares(3)(4))
     assertEquals(Square.White, board.squares(4)(3))
     assertEquals(Square.Black, board.squares(4)(4))
  }


}

