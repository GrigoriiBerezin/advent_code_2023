object implicits {
  implicit class StringOps(val value: String) extends AnyVal {
    def toHash: Int = value.foldLeft(0)((hash, symbol) => (hash + symbol) * 17 % 256)
  }
}
