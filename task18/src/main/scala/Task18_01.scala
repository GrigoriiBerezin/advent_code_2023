object Task18_01 extends Task18 {
  override def fileName: String = "18_input.txt"

  override def calculate(digInfos: Seq[DigInfo]): Long =
    digInfos.foldLeft((Point.start, 1L)) {
      case ((point, areaSize), DigInfo(Direction.Right, length, _)) => (point.copy(x = point.x + length), areaSize + length)
      case ((point, areaSize), DigInfo(Direction.Down, length, _)) => (point.copy(y = point.y + length), areaSize + (point.x + 1) * length.toLong)
      case ((point, areaSize), DigInfo(Direction.Left, length, _)) => (point.copy(x = point.x - length), areaSize)
      case ((point, areaSize), DigInfo(Direction.Up, length, _)) => (point.copy(y = point.y - length), areaSize - point.x * length.toLong)
    }._2
}
