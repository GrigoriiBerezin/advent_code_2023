import implicits.StringOps

object Task15_02 extends Task15 {
  override def fileName: String = "15_input.txt"

  override def calculate: Seq[String] => Long = _.foldLeft(Map.empty[Int, Seq[Pair]]) {
    case (acc, s"$key=$value") =>
      val remappingFunction: Option[Seq[Pair]] => Option[Seq[Pair]] = _.flatMap(seq => value.toIntOption
        .map(value => seq.find(_.key == key) match {
          case Some(_) => seq.updated(seq.indexWhere(_.key == key), Pair(key, value))
          case None => seq :+ Pair(key, value)
        }))
        .orElse(value.toIntOption.map(value => Seq(Pair(key, value))))
      acc.updatedWith(key.toHash)(remappingFunction)
    case (acc, s"$key-") => acc.updatedWith(key.toHash)(_.map(seq => seq.filterNot(_.key == key)))
  }.map {
    case (key, values) => values.map(_.value)
      .zipWithIndex
      .map { case (focus, power) => (key + 1) * focus * (power + 1) }
      .sum
  }
    .sum
}
