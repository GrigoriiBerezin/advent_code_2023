case class Point(x: Int, y: Int) {
  def movedBy(direction: Direction): Point = Point(x + direction.byX, y + direction.byY)
}
