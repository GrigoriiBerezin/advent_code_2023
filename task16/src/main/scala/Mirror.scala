sealed trait Mirror {
  def identifier: Char

  def navigate(dir: Direction): Set[Direction]
}

object Mirror {
  def withName(symbol: Char): Mirror = values.find(_.identifier == symbol).getOrElse(default)

  val values: Seq[Mirror] = Seq(Empty, LeftTiltMirror, RightTiltMirror, VerticalMirror, HorizontalMirror)
  val default: Mirror = Empty

  final case object Empty extends Mirror {
    override def identifier: Char ='.'

    override def navigate(dir: Direction): Set[Direction] = Set(dir)
  }

  final case object LeftTiltMirror extends Mirror {
    val identifier: Char = '\\'

    override def navigate(dir: Direction): Set[Direction] = Set(Direction(dir.byY, dir.byX))
  }

  final case object RightTiltMirror extends Mirror {
    val identifier: Char = '/'

    override def navigate(dir: Direction): Set[Direction] = Set(Direction(-dir.byY, -dir.byX))
  }

  final case object HorizontalMirror extends Mirror {
    override def identifier: Char = '-'

    override def navigate(dir: Direction): Set[Direction] =
      if (dir.byY == 0) Set(dir)
      else Set(Direction(dir.byY, dir.byX), Direction(-dir.byY, dir.byX))
  }

  final case object VerticalMirror extends Mirror {
    override def identifier: Char = '|'

    override def navigate(dir: Direction): Set[Direction] =
      if (dir.byX == 0) Set(dir)
      else Set(Direction(dir.byY, dir.byX), Direction(dir.byY, -dir.byX))
  }
}
