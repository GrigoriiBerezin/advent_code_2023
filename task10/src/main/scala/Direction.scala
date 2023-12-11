sealed trait Direction {
  def opposite: Direction
}

object Direction {
  val values: Seq[Direction] = Seq(Up, Down, Left, Right)

  final case object Up extends Direction {
    override val opposite: Direction = Down
  }
  final case object Down extends Direction {
    override val opposite: Direction = Up
  }
  final case object Left extends Direction {
    override val opposite: Direction = Right
  }
  final case object Right extends Direction {
    override val opposite: Direction = Left
  }
}
