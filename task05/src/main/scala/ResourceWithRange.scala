import DestinationEnum.DestinationEnum

final case class ResourceWithRange(start: Long, end: Long, `type`: DestinationEnum)

object ResourceWithRange {
  def parseSeeds(line: String): Seq[ResourceWithRange] = {
    if (!line.startsWith(seedIdentification)) Seq.empty[ResourceWithRange]
    else {
      line.replace(seedIdentification + ": ", "")
        .split(" ")
        .flatMap(_.toLongOption)
        .grouped(2)
        .map { case Array(start, range) => ResourceWithRange(start, start + range - 1, DestinationEnum.Seed) }
        .toSeq
    }
  }

  private val seedIdentification = "seeds"
}
