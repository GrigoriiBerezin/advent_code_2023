import DestinationEnum.DestinationEnum

final case class Property(startTo: Long, startFrom: Long, range: Long) {
  lazy val endTo: Long = startTo + range - 1
  lazy val endFrom: Long = startFrom + range - 1

  def findDestination(resource: Resource, destinationResource: DestinationEnum): Option[Resource] =
    Option.when(resource.start >= startFrom && resource.start < startFrom + range)(Resource(resource.start + (startTo - startFrom), destinationResource))
}
