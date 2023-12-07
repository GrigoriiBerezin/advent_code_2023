sealed abstract class CardValue(val strRepr: Char)

object CardValue {
  object Ordering {
    implicit val classicOrd: Ordering[CardValue] = ordBase(values)
    implicit val jokerOrd: Ordering[CardValue] =
      ordBase(Seq(Jack, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Queen, King, Ace))

    private def ordBase(values: Seq[CardValue]): Ordering[CardValue] =
      (x: CardValue, y: CardValue) => values.indexOf(x) - values.indexOf(y)
  }

  def apply(char: Char): Option[CardValue] = values.find(_.strRepr == char)

  val values: Seq[CardValue] = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  final object Two extends CardValue('2')
  final object Three extends CardValue('3')
  final object Four extends CardValue('4')
  final object Five extends CardValue('5')
  final object Six extends CardValue('6')
  final object Seven extends CardValue('7')
  final object Eight extends CardValue('8')
  final object Nine extends CardValue('9')
  final object Ten extends CardValue('T')
  final object Jack extends CardValue('J')
  final object Queen extends CardValue('Q')
  final object King extends CardValue('K')
  final object Ace extends CardValue('A')
}
