package model

sealed trait Ratio {
  def symbol: Char
  def operate(limit: Int): Int => Boolean
}

object Ratio {
  final case object LessThan extends Ratio {
    override def symbol: Char = '<'

    override def operate(limit: Int): Int => Boolean = _ < limit
  }
  final case object GreaterThan extends Ratio {
    override def symbol: Char = '>'

    override def operate(limit: Int): Int => Boolean = _ > limit
  }
}
