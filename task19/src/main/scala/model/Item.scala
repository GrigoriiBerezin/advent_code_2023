package model

sealed trait Item

object Item {
  def apply(valueStr: String): Option[Item] = valueStr match {
    case s"$from-$to" => for {
      fromInt <- from.toIntOption
      toInt <- to.toIntOption
    } yield Interval(fromInt, toInt)
    case s"$value" => value.toIntOption.map(Value)
  }

  final case class Value(value: Int) extends Item

  final case class Interval(from: Int, to: Int) extends Item
}