import cats.syntax.option._

trait Direction

object Direction {
  def apply(ch: Char): Option[Direction] = ch match {
    case 'L' => Left.some
    case 'R' => Right.some
    case _ => none[Direction]
  }

  case object Left extends Direction
  case object Right extends Direction
}
