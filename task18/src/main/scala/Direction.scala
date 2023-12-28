sealed trait Direction {
  def identifier: Char
}

object Direction {
  def withNameOption(symbol: Char): Option[Direction] = values.find(_.identifier == symbol)

  val values: Seq[Direction] = Seq(Up, Down, Left, Right)

  case object Up extends Direction {
    val identifier: Char = 'U'
  }

  case object Down extends Direction {
    val identifier: Char = 'D'
  }

  case object Left extends Direction {
    val identifier: Char = 'L'
  }

  case object Right extends Direction {
    val identifier: Char = 'R'
  }
}
