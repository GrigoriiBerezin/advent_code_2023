import cats.Show
import cats.syntax.monoid._
import implicits.FieldsOps

import scala.annotation.tailrec
import scala.collection.mutable

case class RockMap(fields: Vector[Vector[Element]]) {
  def getCoordinatesOf(elem: Element): Seq[Coordinates] = fields.zipWithIndex
    .flatMap { case (elements, yIndex) => elements.zipWithIndex
      .collect { case (e, xIndex) if e.identifier == elem.identifier => Coordinates(xIndex, yIndex) }
    }

  def moveRocks(direction: Direction): RockMap = {
    val movableRocks = getCoordinatesOf(Element.MovableRock)
    direction match {
      case Direction.North | Direction.West => movableRocks.foldLeft(this)((acc, coord) => movedRockMap(acc.fields, coord, direction))
      case Direction.East | Direction.South => movableRocks.foldRight(this)((coord, acc) => movedRockMap(acc.fields, coord, direction))
    }
  }

  lazy val moveCycle: RockMap = RockMap.cycleDirections.foldLeft(this)((acc, direction) => acc.moveRocks(direction))

  lazy val moveUntilCycle: (RockMap, CycleResult) = {
    @tailrec
    def inner(hash: Map[RockMap, Int], countIterations: Int, rockMap: RockMap): (RockMap, CycleResult) = {
      val newMap = rockMap.moveCycle
      hash.get(newMap) match {
        case Some(lastIter) => (newMap, CycleResult(countIterations, countIterations - lastIter))
        case None => inner(hash.updated(newMap, countIterations), countIterations + 1, newMap)
      }
    }
    inner(Map.empty, 1, this)
  }

  private def movedRockMap(fields: Vector[Vector[Element]], coord: Coordinates, direction: Direction): RockMap = {
    @tailrec
    def inner(fields: Vector[Vector[Element]], curCoord: Coordinates): RockMap = {
      val newCoord = curCoord |+| direction.action
      fields.lift(newCoord.y).flatMap(_.lift(newCoord.x)) match {
        case Some(Element.Empty) => inner(fields, newCoord)
        case Some(_: Element.Rock) => RockMap(fields.switchRock(coord, curCoord))
        case None => RockMap(fields.switchRock(coord, curCoord))
      }
    }

    inner(fields, coord)
  }
}

object RockMap {
  implicit val show: Show[RockMap] = (t: RockMap) =>
    t.fields.map(_.map(_.identifier).mkString).mkString(System.lineSeparator())

  val cycleDirections: Seq[Direction] = Seq(Direction.North, Direction.West, Direction.South, Direction.East)

  def build(lines: Seq[String]): RockMap = RockMap(lines.toVector.map(_.map(Element.withName).toVector))
}
