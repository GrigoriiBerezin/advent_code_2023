case class Point(x: Int, y: Int) {
  def move(direction: Direction): Point = direction match {
    case Direction.Up => copy(y = y - 1)
    case Direction.Down => copy(y = y + 1)
    case Direction.Left => copy(x = x - 1)
    case Direction.Right => copy(x = x + 1)
  }
}

object Point {
  val startIdentifier: Char = 'S'

  def findStart(lines: Seq[String]): Option[Point] = {
    lines.zipWithIndex.collectFirst {
      case (line, yIndex) if line.indexOf(startIdentifier) != -1 => Point(line.indexOf(startIdentifier), yIndex)
    }
  }
}
