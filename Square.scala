object Square extends Enumeration {
  val White = Value("W")
  val Black = Value("B")
  val Empty = Value(".")
  val Possible = Value("O")

  def parse(cell:Char):Square.Value = {
    cell match {
      case '.' => Square.Empty
      case 'W' => Square.White
      case 'B' => Square.Black
      case 'O' => Square.Possible
      case _ => throw new RuntimeException
    }
  }

}
