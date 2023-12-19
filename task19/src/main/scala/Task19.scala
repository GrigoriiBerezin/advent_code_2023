import ZioImplicits.ZioOps
import model.{Instructions, Resource}
import zio._

trait Task19 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String

  def calculate(instructions: Instructions, resources: Seq[Resource]): BigInt

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    (rulesRaw, resourcesRaw) = lines.span(_.nonEmpty)
    rules = Instructions.build(rulesRaw)
    resources = resourcesRaw.flatMap(Resource.build)
    result <- calculate(rules, resources).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success
}
