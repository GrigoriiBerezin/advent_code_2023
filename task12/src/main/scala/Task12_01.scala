object Task12_01 extends Task12 {
  override def fileName: String = "12_input.txt"

  override def calculate: HotSpringSchema => Long = _.matchesDefault
}
