import ZioImplicits.ZioOps
import zio._

trait Task08 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def startPattern: String => Boolean
  def endPattern: String => Boolean

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    (pattern, instructions) = getStartConditions(lines.toList)
    result <- instructions.countStepsByPattern(startPattern, endPattern)(pattern.toList).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def getStartConditions(lines: List[String]): (Seq[Direction], MapInstructions) = lines.filter(_.nonEmpty) match {
    case pattern :: instructions => (pattern.flatMap(Direction.apply), MapInstructions(instructions))
    case Nil => (Seq.empty, MapInstructions(Seq.empty))
  }
}
