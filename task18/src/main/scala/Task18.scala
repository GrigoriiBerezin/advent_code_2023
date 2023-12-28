import ZioImplicits.ZioOps
import zio.{System => _, _}

trait Task18 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String

  def calculate(digInfos: Seq[DigInfo]): Long

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    digInfos <- lines.flatMap(DigInfo.apply).succeed
    result <- calculate(digInfos).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success
}
