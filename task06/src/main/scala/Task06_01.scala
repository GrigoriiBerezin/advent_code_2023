import ZioImplicits.ZioOps
import zio._

object Task06_01 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("06_input.txt")
    races <- convertToRaces(lines).succeed
    result <- calculate(races).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def calculate(races: Seq[Race]): Long = races.map(_.winTimes).product

  private def convertToRaces(lines: Seq[String]): Seq[Race] = (for {
    times <- lines.find(_.startsWith("Time:")).map(_.trim.split("\\s+"))
    distances <- lines.find(_.startsWith("Distance:")).map(_.trim.split("\\s+"))
  } yield times.zip(distances).toSeq.flatMap { case (time, distance) => for {
    timeLong <- time.toLongOption
    distanceLong <- distance.toLongOption
  } yield Race(timeLong, distanceLong)
  }).getOrElse(Seq.empty)
}
