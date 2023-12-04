import ZioImplicits.ZioOps
import cats.syntax.monoid._
import zio._

object Task04_02 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines("04_input.txt")
      result <- calculate(lines)
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def calculate(lines: Seq[String]): UIO[Int] = {
    val cards = lines.flatMap(Card.fromString)
    val start = for {
      head <- cards.headOption.map(_.id)
      last <- cards.lastOption.map(_.id)
    } yield (head to last).foldLeft(Map.empty[Int, Int])((acc, value) => acc.updated(value, 1))
    cards.foldLeft(start.getOrElse(Map.empty)) {
      case (acc, card) =>
        val cardCount = acc.getOrElse(card.id, 1)
        (1 to card.winCount).foldLeft(acc) {
          case (acc, cardId) => acc |+| Map((card.id + cardId, cardCount))
        }
    }.values.sum.succeed
  }
}
