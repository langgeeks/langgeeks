
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
    directions.map { |direction| [ direction, view_in_direction(row, col, direction) ] }.to_h
  end

  def view_in_direction(row, col, direction)
    row_step, col_step = steps(direction)
    
    return view = "" if @board[row].nil? || @board[row][col].nil?
    @board[row][col] + view_in_direction(row + row_step, col + col_step, direction) 
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

  def steps(direction)
    @steps ||= {
      :E  => [ 0,  1],
      :SE => [ 1,  1],
      :S  => [ 1,  0],
      :SW => [ 1, -1],
      :W  => [ 0, -1],
      :NW => [-1, -1],
      :N  => [-1,  0],
      :NE => [-1,  1]
    }

    @steps[direction]
  end

  def directions
    [:E, :SE, :S, :SW, :W, :NW, :N, :NE]
  end 
end
