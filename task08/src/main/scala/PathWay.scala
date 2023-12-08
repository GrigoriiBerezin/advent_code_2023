final case class PathWay(left: String, right: String) {
  def goTo(direction: Direction): String = direction match {
    case Direction.Left => left
    case Direction.Right => right
  }
}
