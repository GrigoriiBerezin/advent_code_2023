trait SchemaSymbol {
  def identifier: Char
}

object SchemaSymbol {
  def apply(identifier: Char): SchemaSymbol = values.find(_.identifier == identifier).getOrElse(default)

  val values: Seq[SchemaSymbol] = Seq(Empty, HotSpring, Damaged)
  val default: SchemaSymbol = Damaged

  final case object Empty extends SchemaSymbol {
    override def identifier: Char = '.'
  }

  final case object HotSpring extends SchemaSymbol {
    override def identifier: Char = '#'
  }

  final case object Damaged extends SchemaSymbol {
    override def identifier: Char = '?'
  }
}
