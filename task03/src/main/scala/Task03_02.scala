import cats.syntax.option._
import ZioImplicits.ZioOps
import zio._

object Task03_02 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("03_input.txt")
    numbers <- parseNumbers(lines)
    points <- parsePoints(lines)
    result <- getResult(numbers, points)
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def parseNumbers(lines: Seq[String]): UIO[Seq[NumberWithPosition]] =
    lines.zipWithIndex.foldLeft(Seq.empty[NumberWithPosition]) {
      case (acc, (line, yIndex)) => acc ++ parseNumbersFromLine(line, yIndex)
    }.succeed

  private def parseNumbersFromLine(line: String, yIndex: Int = 0): Seq[NumberWithPosition] = {
    val (acc, cacheNumber) = line.zipWithIndex.foldLeft((Seq.empty[NumberWithPosition], none[NumberWithPosition])) {
      case ((acc, None), (number, xIndex)) if number.isDigit =>
        (acc, NumberWithPosition(number.asDigit, (xIndex - 1, yIndex - 1), (xIndex + 1, yIndex + 1)).some)
      case ((acc, Some(cacheNumber)), (number, xIndex)) if number.isDigit =>
        (acc, cacheNumber.addDigitWithIndex(number.asDigit, (xIndex, yIndex)).some)
      case ((acc, Some(cacheNumber)), (_, _)) => (cacheNumber +: acc, none[NumberWithPosition])
      case ((acc, None), (_, _)) => (acc, none[NumberWithPosition])
    }

    cacheNumber match {
      case Some(value) => value +: acc
      case None => acc
    }
  }

  private def parsePoints(lines: Seq[String]): UIO[Seq[(Int, Int)]] =
    lines.zipWithIndex.foldLeft(Seq.empty[(Int, Int)]) {
      case (acc, (line, yIndex)) => acc ++ parsePointsFromLine(line, yIndex)
    }.succeed

  private def parsePointsFromLine(line: String, yIndex: Int = 0): Seq[(Int, Int)] =
    line.zipWithIndex.foldLeft(Seq.empty[(Int, Int)]) {
      case (acc, ('*', xIndex)) => (xIndex, yIndex) +: acc
      case (acc, _) => acc
    }

  private def getResult(numbers: Seq[NumberWithPosition], points: Seq[(Int, Int)]): UIO[Long] =
    points.view
      .map(point => numbers.filter(_.isNearBy(point)))
      .filter(_.length == 2)
      .map(_.map(_.number).product)
      .sum
      .succeed
}
