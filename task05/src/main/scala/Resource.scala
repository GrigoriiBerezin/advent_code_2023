import DestinationEnum.DestinationEnum

final case class Resource(start: Long, `type`: DestinationEnum)

object Resource {
  def parseSeeds(line: String): Seq[Resource] =
    if (!line.startsWith(seedIdentification)) Seq.empty[Resource]
    else {
      line.replace(seedIdentification + ": ", "")
        .split(" ")
        .flatMap(_.toLongOption.map(start => Resource(start, DestinationEnum.Seed)))
    }

  private val seedIdentification = "seeds"
}
