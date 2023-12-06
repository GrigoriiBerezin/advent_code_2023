import ZioImplicits.ZioOps
import zio._

object Task06_02 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("06_input.txt")
    race <- convertToRace(lines).getOrElse(Race(0, 0)).succeed
    result <- calculate(race).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def calculate(race: Race): Long = race.winTimes

  private def convertToRace(lines: Seq[String]): Option[Race] = for {
    time <- lines.find(_.startsWith("Time:")).map(_.replace("Time:", "").replaceAll("\\s+", "")).flatMap(_.toLongOption)
    distance <- lines.find(_.startsWith("Distance:")).map(_.replace("Distance:", "").replaceAll("\\s+", "")).flatMap(_.toLongOption)
  } yield Race(time, distance)
}
