import ZioImplicits.ZioOps
import zio._

import scala.annotation.tailrec

trait Task09 extends ZIOAppDefault with ReadFileSuite {
  def fileName: String
  def placeFlag: PlaceFlag

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines(fileName)
    values <- lines.map(_.split("\\s+").flatMap(_.toIntOption).toList).succeed
    result <- calculate(values).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def calculate(values: Seq[Seq[Int]]): Long = {
    @tailrec
    def inner(extrapolates: Seq[Int], result: Seq[Int]): Int =
      if (extrapolates.forall(_ == 0)) placeFlag match {
        case PlaceFlag.Start => result.reduce[Int] { case (v1, v2) => v2 - v1 }
        case PlaceFlag.End => result.sum
      }
      else {
        val newExtra = extrapolates.sliding(2).map { case Seq(v1, v2) => v2 - v1 }.toSeq
        val newResult = placeFlag match {
          case PlaceFlag.Start => extrapolates.headOption
          case PlaceFlag.End => extrapolates.lastOption
        }
        inner(newExtra, newResult.getOrElse(0) +: result)
      }

    values.map(inner(_, Seq.empty)).sum
  }
}
