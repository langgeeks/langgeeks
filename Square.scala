object Square extends Enumeration {
  val White = Value("W")
  val Black = Value("B")
  val Empty = Value(".")
  val Possible = Value("O")

  def parse(cell:Char):Square.Value = {
    if (cell == '.')
      return Square.Empty
    if (cell == 'W')
      return Square.White
    if (cell == 'B')
      return Square.Black
    if (cell == 'O')
      return Square.Possible
    throw new RuntimeException
  }

}
