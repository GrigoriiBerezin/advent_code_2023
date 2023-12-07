case class CardHand(cards: Seq[CardValue], bet: Long) {
  self =>
  lazy val cardMap: Map[CardValue, Int] = cards.groupBy(identity).view.mapValues(_.length).toMap
  lazy val combination: Int = {
    val values = cardMap.values
    if (values.exists(_ == 5)) 7
    else if (values.exists(_ == 4)) 6
    else if (values.exists(_ == 3) && values.exists(_ == 2)) 5
    else if (values.exists(_ == 3)) 4
    else if (values.count(_ == 2) == 2) 3
    else if (values.exists(_ == 2)) 2
    else 1
  }
  lazy val replaceJokerWithBest: CardHand =
    if (!cards.contains(CardValue.Jack)) this
    else {
      new CardHand(cards, bet) {
        override lazy val cardMap: Map[CardValue, Int] = {
          val jokerCount = self.cardMap.getOrElse(CardValue.Jack, 0)
          val elementToAdd = self.cardMap.removed(CardValue.Jack).maxByOption { case (_, count) => count }
          elementToAdd.map {
            case (element, value) => self.cardMap.removed(CardValue.Jack).updated(element, value + jokerCount)
          }
            .getOrElse(self.cardMap)
        }
      }
    }
}

object CardHand {
  object Ordering {
    implicit val classicOrd: Ordering[CardHand] = ordBase(CardValue.Ordering.classicOrd)
    implicit val jokerOrd: Ordering[CardHand] = ordBase(CardValue.Ordering.jokerOrd)

    private def ordBase(valueOrdering: Ordering[CardValue]): Ordering[CardHand] = (x: CardHand, y: CardHand) => {
      val byCombination = x.combination - y.combination
      if (byCombination != 0) byCombination
      else x.cards.zip(y.cards)
        .collectFirst {
          case (xC, yC) if xC != yC => valueOrdering.compare(xC, yC)
        }.getOrElse(0)
    }
  }

  def fromStr(line: String): Option[CardHand] = {
    val split = line.split("\\s+")
    for {
      cards <- split.headOption.map(str => str.flatMap(CardValue.apply))
      bet <- split.lastOption.flatMap(_.toIntOption)
    } yield CardHand(cards, bet)
  }
}
