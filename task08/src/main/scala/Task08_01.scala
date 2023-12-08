object Task08_01 extends Task08 {
  override def startPattern: String => Boolean = _ == "AAA"
  override def endPattern: String => Boolean = _ == "ZZZ"

  override def fileName: String = "08_input.txt"
}