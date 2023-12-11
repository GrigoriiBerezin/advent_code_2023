import scala.annotation.tailrec

case class PipeMap(startPoint: Point, map: Map[Point, Pipe]) {

  import PipeMap._

  val waysFromStart: Seq[Seq[PipePoint]] = {
    @tailrec
    def inner(direction: Direction, point: Point, acc: Seq[PipePoint]): Seq[PipePoint] = {
      val pointTo = point.move(direction)
      val pipeTo = map.get(pointTo)
      val nextDirection = pipeTo.flatMap(_.fromToDirections.collectFirst { case (from, to) if from == direction.opposite => to })
      nextDirection match {
        case Some(newDirection) => inner(newDirection, pointTo, acc :+ PipePoint(pointTo, direction.opposite, newDirection))
        case None => acc
      }
    }

    directionsFrom(startPoint).map(direction => inner(direction, startPoint, Seq.empty)).filter(_.nonEmpty)
  }

  def directionsFrom(point: Point): Seq[Direction] = {
    val up = map.get(point.move(Direction.Up)).map(_ => Direction.Up)
    val down = map.get(point.move(Direction.Down)).map(_ => Direction.Down)
    val left = map.get(point.move(Direction.Left)).map(_ => Direction.Left)
    val right = map.get(point.move(Direction.Right)).map(_ => Direction.Right)
    Seq(up, down, left, right).flatten
  }
}

object PipeMap {
  case class PipePoint(coordinates: Point, from: Direction, to: Direction)

  def build(lines: Seq[String]): PipeMap =
    PipeMap(
      Point.findStart(lines).getOrElse(Point(0, 0)),
      lines.zipWithIndex.flatMap {
        case (line, yIndex) => line.zipWithIndex.map { case (symbol, xIndex) => Point(xIndex, yIndex) -> Pipe.parse(symbol) }
      }.toMap
    )
}
