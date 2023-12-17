import cats.Monoid

case class Coordinates(x: Int, y: Int)

object Coordinates {
  implicit val monoid: Monoid[Coordinates] = new Monoid[Coordinates] {
    override def empty: Coordinates = Coordinates(0, 0)

    override def combine(x: Coordinates, y: Coordinates): Coordinates = Coordinates(x.x + y.x, x.y + y.y)
  }
}
