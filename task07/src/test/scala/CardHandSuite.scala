import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CardValue._

class CardHandSuite extends AnyFlatSpec with Matchers {
  "sort" should "sort sequence of a card hands" in {
    val hand1 = CardHand(Jack :: Jack :: Jack :: Jack :: Jack :: Nil, 0)
    val hand2 = CardHand(Jack :: Queen :: Queen :: Six :: Jack :: Nil, 0)
    val hand3 = CardHand(Jack :: Queen :: Queen :: Jack :: Six :: Nil, 0)
    Seq(hand1, hand2, hand3).sorted(CardHand.Ordering.classicOrd) should contain theSameElementsInOrderAs Seq(hand2, hand3, hand1)
  }

  "combination" should "return correct combination number" in {
    val hand = CardHand(Two :: Two :: Four :: King :: King :: Nil, 584)
    hand.combination shouldEqual 3
  }
}
