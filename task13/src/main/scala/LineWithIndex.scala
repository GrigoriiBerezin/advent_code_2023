case class LineWithIndex(line: String, index: Int)

object LineWithIndex {
  def apply(tuple: (String, Int)): LineWithIndex = LineWithIndex(tuple._1, tuple._2 + 1)
}
