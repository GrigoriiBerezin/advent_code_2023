import ZioImplicits.ZioOps
import zio._

object Task04_01 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines("04_input.txt")
      result <- getResult(lines)
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def getResult(lines: Seq[String]): UIO[Long] = {
    def getCountOfWinNumbers(line: String): Int = Card.fromString(line).map(_.winCount).getOrElse(0)

    lines.view.map(getCountOfWinNumbers).map(power => Math.pow(2, power - 1)).map(_.toLong).sum.succeed
  }
}
