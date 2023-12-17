sealed trait Direction {
  def action: Coordinates
}

object Direction {
  final case object North extends Direction {
    val action: Coordinates = Coordinates(0, -1)
  }

  final case object South extends Direction {
    val action: Coordinates = Coordinates(0, 1)
  }

  final case object East extends Direction {
    val action: Coordinates = Coordinates(1, 0)
  }

  final case object West extends Direction {
    val action: Coordinates = Coordinates(-1, 0)
  }
}
