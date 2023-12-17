import ZioImplicits.ZioOps
import zio._

trait Task14 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String

  def rotateMap: RockMap => RockMap

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    rockMap <- RockMap.build(lines).succeed
    finalMap <- rotateMap(rockMap).succeed
    result <- calculate(finalMap).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def calculate(rockMap: RockMap): Int = {
    rockMap
      .getCoordinatesOf(Element.MovableRock)
      .map(_.y)
      .groupBy(y => rockMap.fields.size - y)
      .map { case (perPoint, points) => perPoint * points.length }
      .sum
  }
}
