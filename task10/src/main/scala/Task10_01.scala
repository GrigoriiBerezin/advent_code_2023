object Task10_01 extends Task10 {
  override def fileName: String = "10_input.txt"

  override def calculate: PipeMap => Int = map =>
    map.waysFromStart.collectFirst { case way => (way.length.toDouble / 2).ceil.toInt }.getOrElse(-1)
}
