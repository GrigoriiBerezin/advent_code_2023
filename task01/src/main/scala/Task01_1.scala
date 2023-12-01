import zio._

object Task01_1 extends ZIOAppDefault with ReadFileSuite with CalculateBase {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("01_input.txt")
    result <- calculate(lines)
    _ <- Console.printLine(result)
  } yield ExitCode.success

  override protected def getNumber(line: String): Option[Long] = for {
    first <- line.find(_.isDigit)
    last <- line.findLast(_.isDigit)
  } yield first.asDigit * 10 + last.asDigit
}
