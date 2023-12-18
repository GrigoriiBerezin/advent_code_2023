import implicits.StringOps

object Task15_01 extends Task15 {
  override def fileName: String = "15_input.txt"

  override def calculate: Seq[String] => Long = _.map(_.toHash).sum
}
