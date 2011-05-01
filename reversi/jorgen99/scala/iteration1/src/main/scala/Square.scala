object Square extends Enumeration {
  val White = Value("W")
  val Black = Value("B")
  val Empty = Value(".")
  val Possible = Value("O")

  def apply(cell: Char): Square.Value = cell match {
    case '.' => Empty
    case 'W' => White
    case 'B' => Black
    case 'O' => Possible
    case _ => throw new RuntimeException
  }

}
