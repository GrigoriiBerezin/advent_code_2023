import DestinationEnum.DestinationEnum
import cats.syntax.option._

final case class ResourceMap(from: DestinationEnum, to: DestinationEnum, properties: Seq[Property]) {
  def findDestinationResource(resource: Resource): Option[Resource] =
    Option.when(resource.`type` == from)(
      properties.flatMap(property => property.findDestination(resource, to))
        .headOption.getOrElse(resource.copy(`type` = to))
    )

  def findDestinationResourceWithRange(resource: ResourceWithRange): Seq[ResourceWithRange] = {
    val sortedProperties = properties.sortBy(_.startFrom)
    val (newResources, leftResource) = sortedProperties.foldLeft((Seq.empty[ResourceWithRange], resource.some)) {
      case ((acc, Some(leftResource)), prop) =>
        val underRange = Option.when(leftResource.start < prop.startFrom)(ResourceWithRange(leftResource.start, Math.min(prop.startFrom - 1, leftResource.end), to))
        val inRange = Option.when(leftResource.start >= prop.startFrom && leftResource.start <= prop.endFrom ||
          leftResource.end >= prop.startFrom && leftResource.end <= prop.endFrom ||
          leftResource.start <= prop.startFrom && leftResource.end >= prop.endFrom) {
          val delay = prop.startTo - prop.startFrom
          ResourceWithRange(Math.max(leftResource.start, prop.startFrom) + delay, Math.min(leftResource.end, prop.endFrom) + delay, to)
        }
        val aboveRange = Option.when(leftResource.end > prop.endFrom)(ResourceWithRange(Math.max(leftResource.start, prop.endFrom + 1), leftResource.end, to))
        (Seq(underRange, inRange, acc).flatten, aboveRange)
      case ((acc, None), _) => (acc, None)
    }
    Seq(newResources, leftResource).flatten
  }
}

object ResourceMap {
  type BuildDefinition = (String, Seq[String])

  def buildFromLines(lines: Seq[String]): Seq[ResourceMap] = {
    lines.filter(line => (line.endsWith(mapIdentification + ":") || line.forall(ch => ch.isDigit || ch.isSpaceChar)) && !line.isBlank)
      .foldLeft(Seq.empty[BuildDefinition]) {
        case (acc, line) if line.endsWith(mapIdentification + ":") => (line, Seq.empty) +: acc
        case (Seq((definition, properties), last@_*), line) => (definition, line +: properties) +: last
      }
      .flatMap {
        case (definition, properties) => build(definition, properties)
      }
  }

  def build(definitionLine: String, propertyLines: Seq[String]): Option[ResourceMap] = {
    val destinationsRow = definitionLine.replace(mapIdentification + ":", "").trim.split("-to-")
    val properties = propertyLines.map(line => line.split(" ").flatMap(_.toLongOption)).collect {
      case Array(startFrom, startTo, range) => Property(startFrom, startTo, range)
    }
    for {
      from <- destinationsRow.headOption.map(str => DestinationEnum.withName(str.capitalize))
      to <- destinationsRow.lastOption.map(str => DestinationEnum.withName(str.capitalize))
    } yield ResourceMap(from, to, properties)
  }

  private val mapIdentification = "map"
}

