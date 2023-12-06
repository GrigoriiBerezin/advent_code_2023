import ZioImplicits.ZioOps
import zio._

object Task05_02 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines("05_input.txt")
      seeds <- lines.find(_.startsWith("seeds:")).map(ResourceWithRange.parseSeeds).getOrElse(Seq.empty).succeed
      maps <- ResourceMap.buildFromLines(lines).succeed
      result <- calculate(seeds, maps).succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def calculate(seeds: Seq[ResourceWithRange], maps: Seq[ResourceMap]): Long = {
    def inner(resource: ResourceWithRange): Seq[ResourceWithRange] =
      maps.find(_.from == resource.`type`) match {
        case Some(map) => map.findDestinationResourceWithRange(resource).flatMap {
          case _ if resource.`type` == DestinationEnum.Location => Seq(resource)
          case newResource => inner(newResource)
        }
        case None => Seq(resource)
      }

    seeds.flatMap(inner).minBy(_.start).start
  }
}
