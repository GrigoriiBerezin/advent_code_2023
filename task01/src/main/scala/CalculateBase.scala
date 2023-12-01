import cats.syntax.monoid._
import cats.Monoid
import zio._

trait CalculateBase {
  protected def getNumber(str: String): Option[Long]

  protected def calculate(lines: Seq[String]): UIO[Long] =
    ZIO.succeed(lines.foldLeft(Monoid[Long].empty)((acc, line) => getNumber(line).map(_ |+| acc).getOrElse(acc)))
}
