import ZioImplicits.ZioOps
import zio._

trait Task11 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def valueOfEmptyGalaxy: Int

  override def run: ZIO[Environment, Any, Any] =
    for {
      lines <- getLines(fileName)
      galaxy <- Galaxy.build(lines, valueOfEmptyGalaxy).succeed
      result <- calculate(galaxy).succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def calculate(galaxy: Galaxy): Long = {
    def inner(stars: Seq[Star]): Seq[Int] = {
      stars match {
        case Seq(star, tail @ _*) => tail.map(star2 => galaxy.distanceBetweenStars(star, star2)) ++ inner(tail)
        case Seq() => Seq.empty
      }
    }
    inner(galaxy.stars).map(_.toLong).sum
  }
}
