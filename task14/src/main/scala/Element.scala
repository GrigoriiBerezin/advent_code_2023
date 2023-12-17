sealed trait Element {
  def identifier: Char
}

object Element {
  def withName(symbol: Char): Element = values.find(_.identifier == symbol).getOrElse(default)

  val values: Set[Element] = Set(Empty, StableRock, MovableRock)
  val default: Element = Empty

  final case object Empty extends Element {
    val identifier: Char = '.'
  }

  sealed trait Rock extends Element

  final case object StableRock extends Rock {
    val identifier: Char = '#'
  }

  final case object MovableRock extends Rock {
    val identifier: Char = 'O'
  }
}
