
def strip(board)
  board.lines.
    map { |line| line.strip }.
    reject { |line| line.blank? }.
    join("\n")
end

class Reversi
  def initialize(board)
    @board = strip(board)
  end

  def legal_moves
    @board
  end

  
end
