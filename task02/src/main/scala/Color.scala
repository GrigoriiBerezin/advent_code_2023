sealed abstract class Color(val identifier: String)

object Color {
  final case object Red extends Color("red")
  final case object Green extends Color("green")
  final case object Blue extends Color("blue")

  def fromStr(str: String): Option[Color] = converter.collectFirst { case (`str`, color) => color }

  private def converter: Seq[(String, Color)] = Seq(Red, Green, Blue).map(color => (color.identifier, color))
}
