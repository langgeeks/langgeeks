require 'reversi'
require 'facets'

describe Reversi do

  before do
    @board = "
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

    @reversi = Reversi.new(@board)
  end 
    

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
   
      board = strip(board)
      legal_moves = strip(legal_moves)
      
      Reversi.new(board).legal_moves.should == legal_moves 
    end
  end


  it "should know what the board looks like in all directions from a given position" do
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

    Reversi.new(board).views_and_directions(1, 3).should == {
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

  it "should know the view in a certain direction" do
    @reversi.view_in_direction(1, 3,  :E).should == ".W..."
    @reversi.view_in_direction(1, 3, :NW).should == ".."
  end
end

