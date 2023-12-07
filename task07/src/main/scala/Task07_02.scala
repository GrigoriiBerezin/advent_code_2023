import ZioImplicits.ZioOps
import CardHand.Ordering.jokerOrd
import zio._

object Task07_02 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("07_input.txt")
    cards <- lines.flatMap(CardHand.fromStr).map(_.replaceJokerWithBest).succeed
    result <- calculate(cards).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def calculate(cardHands: Seq[CardHand]): Long =
    cardHands.sorted.zipWithIndex.map { case (hand, multiply) => hand.bet * (multiply + 1) }.sum
}
