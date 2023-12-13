import scala.collection.mutable

case class HotSpringSchema(hotSpringsView: Seq[SchemaSymbol], hotSpringsList: Seq[Int]) {
  private val cache = mutable.Map.empty[(Seq[SchemaSymbol], Seq[Int], Int), Long]

  def matches(states: Seq[SchemaSymbol], counts: Seq[Int], seen: Int): Long = {
    def damaged(leftStates: Seq[SchemaSymbol]): Long = if (counts.isEmpty) 0 else matches(leftStates, counts, seen + 1)

    def empty(leftStates: Seq[SchemaSymbol]): Long = (seen, counts) match {
      case (0, _) => matches(leftStates, counts, 0)
      case (_, `seen` :: cTail) => matches(leftStates, cTail, 0)
      case _ => 0
    }

    cache.getOrElseUpdate((states, counts, seen), states.toList match {
      case Nil if (seen == 0 && counts.isEmpty) || counts == Seq(seen) => 1
      case Nil => 0
      case SchemaSymbol.Damaged :: tail => empty(tail) + damaged(tail)
      case SchemaSymbol.HotSpring :: tail => damaged(tail)
      case SchemaSymbol.Empty :: tail => empty(tail)
    })
  }

  lazy val matchesDefault: Long = matches(hotSpringsView, hotSpringsList, 0)
}

object HotSpringSchema {
  def from(line: String): HotSpringSchema = {
    line match {
      case s"$schema $list" => HotSpringSchema(
        schema.map(SchemaSymbol.apply),
        list.split(',').flatMap(_.toIntOption)
      )
      case _ => HotSpringSchema(Seq.empty, Seq.empty)
    }
  }
}
