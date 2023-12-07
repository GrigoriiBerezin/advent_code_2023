import ZioImplicits.ZioOps
import CardHand.Ordering.classicOrd
import zio._

object Task07_01 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("07_input.txt")
    cardHands <- getCardHands(lines).succeed
    result <- calculate(cardHands).succeed
    _ <- Console.printLine(result)
  } yield ExitCode.success

  private def getCardHands(lines: Seq[String]): Seq[CardHand] = lines.flatMap(CardHand.fromStr)

  private def calculate(cardHands: Seq[CardHand]): Long =
    cardHands.sorted.zipWithIndex.map { case (hand, multiply) => hand.bet * (multiply + 1) }.sum
}
