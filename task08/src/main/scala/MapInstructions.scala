import scala.annotation.tailrec

final case class MapInstructions(instructions: Map[String, PathWay]) {
  def countStepsByPattern(startF: String => Boolean, endF: String => Boolean)(pattern: List[Direction]): BigInt = {
    @tailrec
    def inner(directions: List[Direction], currentNode: PathWay, steps: Int): Int = {
      directions match {
        case dir :: left => currentNode.goTo(dir) match {
          case way if endF(way) => steps + 1
          case way => inner(left, instructions(way), steps + 1)
        }
        case Nil => inner(pattern, currentNode, steps)
      }
    }

    @tailrec
    def gdc(x: BigInt, y: BigInt): BigInt =
      if (y == 0) x else gdc(y, x % y)

    def lcm(x: BigInt, y: BigInt): BigInt =
      (x * y) / gdc(x, y)

    instructions.keys
      .view
      .filter(startF)
      .flatMap(instructions.get)
      .map(inner(pattern, _, 0))
      .map(BigInt.apply)
      .foldLeft(BigInt(1))((acc, steps) => lcm(acc, steps))
  }
}

object MapInstructions {
  def apply(instructions: Seq[String]): MapInstructions = MapInstructions(instructions.foldLeft(Map.empty[String, PathWay]) {
    case (acc, s"$node = ($left, $right)") => acc.updated(node, PathWay(left, right))
    case (acc, _) => acc
  })
}
