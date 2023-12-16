import ZioImplicits.ZioOps
import zio._

trait Task13 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String

  def threshold: Int
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines(fileName)
      rowMaps <- collectRawMaps(lines).succeed
      maps <- rowMaps.map(MirrorsMap.from).succeed
      result <- maps.map(calculate).sum.succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def collectRawMaps(lines: Seq[String]): Seq[Seq[String]] = {
    val (maps, restMap) = lines.foldRight((Seq.empty[Seq[String]], Seq.empty[String])) {
      case (line, (acc, currMap)) if line.nonEmpty => (acc, line +: currMap)
      case (_, (acc, currMap)) => (currMap +: acc, Seq.empty[String])
    }
    if (restMap.isEmpty) maps
    else restMap +: maps
  }

  private def calculate(map: MirrorsMap): Int = {
    val vC = map.verticalCentres(threshold).map(_.leftIndex).sum
    val hC = map.horizontalCentres(threshold).map(_.leftIndex * 100).sum
    vC + hC
  }
}
