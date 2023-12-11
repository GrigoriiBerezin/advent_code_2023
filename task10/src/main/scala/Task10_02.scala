import PipeMap.PipePoint

import scala.annotation.tailrec

object Task10_02 extends Task10 {
  override def fileName: String = "10_input.txt"

  override def calculate: PipeMap => Int = map => {
    def setRotate(from: Direction, to: Direction): Option[Direction] = (from, to) match {
      case (Direction.Left, Direction.Up) => Some(Direction.Left)
      case (Direction.Left, Direction.Down) => Some(Direction.Right)
      case (Direction.Right, Direction.Up) => Some(Direction.Right)
      case (Direction.Right, Direction.Down) => Some(Direction.Left)
      case (Direction.Down, Direction.Left) => Some(Direction.Left)
      case (Direction.Down, Direction.Right) => Some(Direction.Right)
      case (Direction.Up, Direction.Left) => Some(Direction.Right)
      case (Direction.Up, Direction.Right) => Some(Direction.Left)
      case _ => None
    }

    def collectInners(map: Seq[PipePoint], direction: Direction, coord: Point, from: Direction, to: Direction): Set[Point] = {
      def setMovement(direction: Direction, from: Direction, to: Direction): Seq[Direction] = (direction, from, to) match {
        case (Direction.Right, Direction.Left, Direction.Right) => Seq(Direction.Down)
        case (Direction.Right, Direction.Left, Direction.Up) => Seq(Direction.Down, Direction.Right)
        case (Direction.Right, Direction.Right, Direction.Left) => Seq(Direction.Up)
        case (Direction.Right, Direction.Right, Direction.Down) => Seq(Direction.Left, Direction.Up)
        case (Direction.Right, Direction.Up, Direction.Down) => Seq(Direction.Left)
        case (Direction.Right, Direction.Up, Direction.Right) => Seq(Direction.Left, Direction.Down)
        case (Direction.Right, Direction.Down, Direction.Up) => Seq(Direction.Right)
        case (Direction.Right, Direction.Down, Direction.Left) => Seq(Direction.Right, Direction.Up)
        case _ => Seq.empty
      }

      @tailrec
      def inner(point: Point, directionTo: Direction, acc: Set[Point]): Set[Point] = {
        val nextPoint = point.move(directionTo)
        map.find(_.coordinates == nextPoint) match {
          case Some(_) => acc
          case None => inner(nextPoint, directionTo, acc + nextPoint)
        }
      }

      setMovement(direction, from, to).foldLeft(Set.empty[Point]) { case (acc, directionTo) => acc ++ inner(coord, directionTo, Set.empty) }
    }

    val way = map.waysFromStart.headOption.getOrElse(Seq.empty)
    val direction = way.flatMap(p => setRotate(p.from, p.to)).groupBy(identity).maxBy { case (_, v) => v.length }._1
    val points = way.foldLeft(Set.empty[Point]) {
      case (acc, PipePoint(coord, from, to)) => acc ++ collectInners(way, direction, coord, from, to)
    } - map.startPoint
    points.size
  }
}
