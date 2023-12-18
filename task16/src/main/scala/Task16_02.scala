object Task16_02 extends Task16 {
  override def fileName: String = "16_input.txt"

  override def calculate(mirrors: Seq[Seq[Mirror]]): Int = {
    val ySize = mirrors.length
    val xSize = mirrors.headOption.map(_.length).getOrElse(0)
    val fromLeft = (0 until ySize).map(y => countEnergizedFields(mirrors, Point(0, y), Direction(1, 0)))
    val fromRight = (0 until ySize).map(y => countEnergizedFields(mirrors, Point(xSize - 1, y), Direction(-1, 0)))
    val fromTop = (0 until xSize).map(x => countEnergizedFields(mirrors, Point(x, 0), Direction(0, 1)))
    val fromBottom = (0 until xSize).map(x => countEnergizedFields(mirrors, Point(x, ySize - 1), Direction(0, -1)))
    Seq(fromLeft, fromRight, fromTop, fromBottom).flatten.max
  }
}
