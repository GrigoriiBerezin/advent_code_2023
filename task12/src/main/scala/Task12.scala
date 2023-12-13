import ZioImplicits.ZioOps
import zio._

trait Task12 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def calculate: HotSpringSchema => Long

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines(fileName)
      schemas <- lines.map(HotSpringSchema.from).succeed
      result <- schemas.map(calculate).sum.succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success
}
