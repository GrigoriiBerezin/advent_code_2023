final case class Card(id: Int, owns: Seq[Int], winners: Seq[Int]) {
  lazy val wins: Seq[Int] = owns.filter(winners.contains)
  lazy val winCount: Int = wins.size
}

object Card {
  def fromString(s: String): Option[Card] = {
    val split = s.split(':')
    for {
      id <- split.headOption.map(_.replace("Card", "").trim.toInt)
      (owns, winners) <- split.lastOption.flatMap(getNumbers)
    } yield Card(id, owns, winners)
  }

  private def getNumbers(line: String): Option[(Seq[Int], Seq[Int])] = {
    val split = line.split('|')
    for {
      owns <- split.headOption.map(_.trim.split(' ').filter(_.nonEmpty).map(_.toInt))
      winners <- split.lastOption.map(_.trim.split(' ').filter(_.nonEmpty).map(_.toInt))
    } yield (owns, winners)
  }
}
