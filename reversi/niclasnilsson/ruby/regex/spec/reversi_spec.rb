require 'reversi'
require 'facets'

describe Reversi do

  { "
      ........
      ........
      ........
      ...BW...
      ...WB...
      ........
      ........
      ........
      B
   "  =>

   "
      ........
      ........
      ....0...
      ...BW0..
      ..0WB...
      ...0....
      ........
      ........",

   "
      W.......
      .WB.W...
      ..BB.B..
      ..BWBW..
      ..BBW...
      .BBBWB..
      ....W.B.
      ........
      W
   "  =>

   "
      W.......
      .WB0W0..
      .0BB0B..
      .0BWBW0.
      .0BBW.0.
      0BBBWB0.
      0.00W.B.
      .......0
   "

  }.each do |board, legal_moves|


    it "
    A board like this:
      #{board} 
    should result in these legal moves: 
      #{legal_moves}" do 
   
      pending 
      board = strip(board)
      legal_moves = strip(legal_moves)
      
      Reversi.new(board).legal_moves.should == legal_moves 
    end
  end


  it "should know what the board looks like in all directions from a given position" do
    pending

    board = strip "
      W.......
      .WB.W...
      ..BB.B..
      ..BWBW..
      ..BBW...
      .BBBWB..
      ....W.B.
      ........
      W
    "

    Reversi.new(board).views_from_position(1, 3).should == {
      :E  => ".W...",
      :SE => "..W..",
      :S  => ".BWBB..",
      :SW => ".B..",
      :W  => ".BW.",
      :NW => "..",
      :N  => "..",
      :NE => ".."
    }

  end

end

