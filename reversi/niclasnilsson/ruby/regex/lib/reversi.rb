
def strip(board)
  board.lines.
    map { |line| line.strip }.
    reject { |line| line.blank? }.
    join("\n")
end

class Reversi
  def initialize(board)
    @board = tokenize_board(board)
  end

  def legal_moves
    nil 
  end

  def views_from_position(row, col)

  end

  def [](row, col)
    @board[row][col]
  end

  private

  def tokenize_board(board_str)
    strip(board_str).
      split[0..7].
      map { |row| row.split("") }
  end

end
