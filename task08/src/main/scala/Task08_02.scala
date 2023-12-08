object Task08_02 extends Task08 {
  override def startPattern: String => Boolean = _.endsWith("A")
  override def endPattern: String => Boolean = _.endsWith("Z")

  override def fileName: String = "08_input.txt"
}
