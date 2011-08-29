import org.specs2.mutable._
import scala.util._
import scala.collection.mutable.{ListBuffer}
import Reversi._

class ReversiSpec extends Specification {



  "The Game" should {

    val board =  """
                       W.......
                       .WB.W...
                       ..BB.B..
                       ..BWBW..
                       ..BBW...
                       .BBBWB..
                       ....W.B.
                       ........
                       W
    """

    val game = Reversi(board)

    "be able to parse the board" in {
      game.player must_== 'W'
      game.board(0)(0) must_== 'W'
      game.board(6)(7) must_== '.'
      game.board(2)(5) must_== 'B'
    }

    "know the view in one direction" in {
      game.viewInDirection(1, 3, 'E) must_== ".W..."
    }

    "know the view in all directions" in {
      game.viewsInAllDirections(1, 3) must havePairs (
        'E  -> ".W...",
        'SE -> "..W..",
        'S  -> ".BWBB..",
        'SW -> ".B..",
        'W  -> ".BW.",
        'NW -> "..",
        'N  -> "..",
        'NE -> ".."
      )
    }

    "know the legal moves in one direction" in {
      game.legalInOneDirection(".BW") must_== true
      game.legalInOneDirection(".BBBW") must_== true
      game.legalInOneDirection(".WB") must_== false
      game.legalInOneDirection("BW") must_== false
      game.legalInOneDirection("BBBW") must_== false
      game.legalInOneDirection(".B.") must_== false
      game.legalInOneDirection(".B.W") must_== false
      game.legalInOneDirection(".B..W") must_== false
      game.legalInOneDirection("..BW") must_== false
    }

    "be able to clone the board to a mutable representation" in  {
      val mutable = game.toListBuffer
      mutable must have size(8)
      mutable(0)(0) must_== 'W'
      mutable(6)(7) must_== '.'
      mutable(2)(5) must_== 'B'
    }

    "The final test" in {
      val expectedBoard = """

              W.......
              .WB0W0..
              .0BB0B..
              .0BWBW0.
              .0BBW.0.
              0BBBWB0.
              0.00W.B.
              .......0
              W

      """
      game.legalMoves must_== Reversi.strip(expectedBoard)
    }
  }

}

