import ZioImplicits.ZioOps
import zio._

trait Task16 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def calculate(mirrors: Seq[Seq[Mirror]]): Int

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    mirrors <- lines.map(line => line.map(symbol => Mirror.withName(symbol))).succeed
    result <- calculate(mirrors).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  def countEnergizedFields(fields: Seq[Seq[Mirror]], start: Point, startDirection: Direction): Int = {
    def inner(point: Point, direction: Direction, acc: Map[Point, Seq[Direction]]): Map[Point, Seq[Direction]] = {
      fields.lift(point.y).flatMap(_.lift(point.x)) match {
        case Some(_) if acc.get(point).exists(_.contains(direction)) => acc
        case Some(mirror) =>
          val withCurrentVisit = acc.updatedWith(point)(_.map(_.prepended(direction)).orElse(Some(Seq(direction))))
          mirror.navigate(direction).foldLeft(withCurrentVisit)((acc, direction) => inner(point.movedBy(direction), direction, acc))
        case None => acc
      }
    }

    inner(start, startDirection, Map.empty).keys.size
  }
}
