object Task16_01 extends Task16 {
  override def fileName: String = "16_input.txt"

  override def calculate(mirrors: Seq[Seq[Mirror]]): Int = {
    val start = Point(0, 0)
    val startDirection = Direction(1, 0)
    countEnergizedFields(mirrors, start, startDirection)
  }
}
