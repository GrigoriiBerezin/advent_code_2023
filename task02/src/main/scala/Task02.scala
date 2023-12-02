import zio._

trait Task02 {
  self: ZIOAppDefault with ReadFileSuite =>

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("02_input.txt")
    games = lines.flatMap(GameEntity.fromStr)
    result <- calculate(games)
    _ <- Console.printLine(result)
  } yield ExitCode.success

  protected def calculate(games: Seq[GameEntity]): UIO[Long]
}
