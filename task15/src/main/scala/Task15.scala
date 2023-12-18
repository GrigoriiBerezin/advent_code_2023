import ZioImplicits.ZioOps
import zio._

trait Task15 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String

  def calculate: Seq[String] => Long

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines(fileName)
      result <- calculate(lines.flatMap(_.split(','))).succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success


}
