sealed trait Pipe {
  def identifier: Char
  def fromToDirections: Seq[(Direction, Direction)]
}

object Pipe {
  def parse(symbol: Char): Pipe = values.find(_.identifier == symbol).getOrElse(default)

  val values: Seq[Pipe] = Seq(F, `7`, L, J, `|`, `-`, Empty)
  val default: Pipe = Empty

  final case object F extends Pipe {
    override def identifier: Char = 'F'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Down -> Direction.Right, Direction.Right -> Direction.Down)
  }
  final case object `7` extends Pipe {
    override def identifier: Char = '7'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Left -> Direction.Down, Direction.Down -> Direction.Left)
  }
  final case object L extends Pipe {
    override def identifier: Char = 'L'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Up -> Direction.Right, Direction.Right -> Direction.Up)
  }
  final case object J extends Pipe {
    override def identifier: Char = 'J'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Up -> Direction.Left, Direction.Left -> Direction.Up)
  }
  final case object `|` extends Pipe {
    override def identifier: Char = '|'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Up -> Direction.Down, Direction.Down -> Direction.Up)
  }
  final case object `-` extends Pipe {
    override def identifier: Char = '-'
    override def fromToDirections: Seq[(Direction, Direction)] =
      Seq(Direction.Left -> Direction.Right, Direction.Right -> Direction.Left)
  }
  final case object Empty extends Pipe {
    override def identifier: Char = '.'
    override def fromToDirections: Seq[(Direction, Direction)] = Seq.empty
  }
}
