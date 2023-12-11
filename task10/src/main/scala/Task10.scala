import ZioImplicits.ZioOps
import zio._

trait Task10 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def calculate: PipeMap => Int

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines(fileName)
      map <- PipeMap.build(lines).succeed
      result <- calculate(map).succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success
}
