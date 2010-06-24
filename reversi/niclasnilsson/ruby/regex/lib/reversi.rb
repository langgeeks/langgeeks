
def strip(board)
  board.lines.
    map { |line| line.strip }.
    reject { |line| line.blank? }.
    join("\n")
end

class Reversi
  def initialize(board)
    @board = tokenize_board(board)
    @player = board.split.last
  end

  def legal_moves
    board = @board.clone

    all_positions.each do |row, col|
      board[row][col] = "0" if legal_move(row, col)
    end

    board_to_s(board)
  end

  def views_and_directions(row, col)
    directions.map { |direction| [ direction, view_in_direction(row, col, direction) ] }.to_h
  end

  def view_in_direction(row, col, direction)
    row_step, col_step = steps_in(direction)
   
    return "" if row < 0 || col < 0 
    return "" if @board[row].nil? || @board[row][col].nil?
    @board[row][col] + view_in_direction(row + row_step, col + col_step, direction) 
  end

  private

  def tokenize_board(board_str)
    strip(board_str).
      split[0..7].
      map { |row| row.split("") }
  end

  def board_to_s(board)
    board.map { |row| row.join }.join("\n")
  end
   
  def steps 
    {
      :E  => [ 0,  1],
      :SE => [ 1,  1],
      :S  => [ 1,  0],
      :SW => [ 1, -1],
      :W  => [ 0, -1],
      :NW => [-1, -1],
      :N  => [-1,  0],
      :NE => [-1,  1]
    }
  end

  def steps_in(direction)
    steps[direction]
  end

  def directions
    [:E, :SE, :S, :SW, :W, :NW, :N, :NE]
  end 

  def views(row, col)
    views_and_directions(row, col).values
  end
  
  def player 
    @player
  end

  def opponent
    @player == "W" ? "B" : "W"
  end

  def all_positions
    range = (0..7).to_a
    range.product(range)
  end

  def legal_move(row, col)
    pattern = /^\.#{opponent}+#{player}/
    views(row, col).any? { |view| view =~ pattern }
  end  

end
