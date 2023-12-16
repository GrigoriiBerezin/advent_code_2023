import scala.annotation.tailrec

case class MirrorsMap(horizontalMatches: Set[CompareInfo], verticalMatches: Set[CompareInfo], xSize: Int, ySize: Int) {
  def verticalCentres(threshold: Int): Set[CompareInfo] = findCentres(verticalMatches, ySize, threshold)

  def horizontalCentres(threshold: Int): Set[CompareInfo] = findCentres(horizontalMatches, xSize, threshold)

  private def findCentres(matches: Set[CompareInfo], limitSize: Int, threshold: Int): Set[CompareInfo] = {
    val potentialEqualLines = matches.filter(info => info.diffCount <= threshold)
    val potentialStarts = potentialEqualLines.filter(info => info.rightIndex - info.leftIndex == 1)
    potentialStarts.filter(info => isStart(info, potentialEqualLines, limitSize, threshold - info.diffCount))
  }

  private def isStart(potentialStart: CompareInfo, equalPairs: Set[CompareInfo], limitSize: Int, threshold: Int): Boolean = {
    potentialStart.leftIndex == 1 && (threshold == 0 || potentialStart.diffCount == threshold) ||
      potentialStart.rightIndex == limitSize && (threshold == 0 || potentialStart.diffCount == threshold) ||
      equalPairs.filter(info => info.leftIndex == potentialStart.leftIndex - 1 && info.rightIndex == potentialStart.rightIndex + 1)
        .exists(info => isStart(info, equalPairs, limitSize, threshold - info.diffCount))
  }
}

object MirrorsMap {
  def from(lines: Seq[String]): MirrorsMap = {
    val indexedLines = lines.toVector.map(_.toVector)
    val horizontals = makeMatches(lines)
    val (verticalLines, ySize) = lines match {
      case Seq(head, _@_*) => (head.indices.foldRight(Seq.empty[String]) {
        case (index, acc) => indexedLines.map(_ (index)).mkString("") +: acc
      }, head.length)
      case _ => (Seq.empty, 0)
    }
    val verticals = makeMatches(verticalLines)
    MirrorsMap(horizontals, verticals, lines.size, ySize)
  }

  private def makeMatches(lines: Seq[String]): Set[CompareInfo] = {
    def buildCompare(compareInfo: LineWithIndex, leftLines: Seq[LineWithIndex]): Seq[CompareInfo] =
      leftLines.map(left => CompareInfo(compareInfo.index, left.index, left.line.zip(compareInfo.line).count { case (v1, v2) => v1 != v2 }))

    @tailrec
    def inner(linesWithIndex: Seq[LineWithIndex], acc: Seq[CompareInfo] = Seq.empty): Seq[CompareInfo] = {
      linesWithIndex match {
        case Seq(compareLine, leftLines@_*) => inner(leftLines, buildCompare(compareLine, leftLines) ++ acc)
        case Seq() => acc
      }
    }

    inner(lines.zipWithIndex.map(LineWithIndex.apply)).toSet
  }
}
