final case class DigInfo(direction: Direction, length: Int, byColor: Option[DigInfo])

object DigInfo {
  def apply(lineInfo: String): Option[DigInfo] = lineInfo match {
    case s"$direction $moves (#$color)" => for {
      direction <- Direction.withNameOption(direction.charAt(0))
      moves <- moves.toIntOption
      colorDirection <- color.lastOption.flatMap(charToDirection.get)
      colorLength = Integer.parseInt(color.dropRight(1), 16)
      byColor = Some(DigInfo(colorDirection, colorLength, None))
    } yield DigInfo(direction, moves, byColor)
    case _ => None
  }

  private val charToDirection: Map[Char, Direction] = Map(
    '0' -> Direction.Right,
    '1' -> Direction.Down,
    '2' -> Direction.Left,
    '3' -> Direction.Up
  )
}
