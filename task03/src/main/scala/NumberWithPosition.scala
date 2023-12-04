final case class NumberWithPosition(number: Long, leftPoint: (Int, Int), rightPoint: (Int, Int)) {
  def addDigitWithIndex(digit: Int, index: (Int, Int)): NumberWithPosition =
    NumberWithPosition(number * 10 + digit, leftPoint, (index._1 + 1, index._2 + 1))

  def isNearBy(point: (Int, Int)): Boolean =
    point._1 >= leftPoint._1 &&
    point._1 <= rightPoint._1 &&
      point._2 >= leftPoint._2 &&
      point._2 <= rightPoint._2
}