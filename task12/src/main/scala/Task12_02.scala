object Task12_02 extends Task12 {
  override def fileName: String = "12_input.txt"

  override def calculate: HotSpringSchema => Long = schema => {
    schema.matches(
      Seq.fill(5)(SchemaSymbol.Damaged +: schema.hotSpringsView).flatten.tail,
      Seq.fill(5)(schema.hotSpringsList).flatten,
      0
    )
  }
}
