import ZioImplicits.ZioOps
import zio._

import scala.annotation.tailrec

object Task05_01 extends ZIOAppDefault with ReadFileSuite {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    for {
      lines <- getLines("05_input.txt")
      seeds <- lines.find(_.startsWith("seeds:")).map(Resource.parseSeeds).getOrElse(Seq.empty).succeed
      maps <- ResourceMap.buildFromLines(lines).succeed
      result <- calculate(seeds, maps).succeed
      _ <- Console.printLine(result)
    } yield ExitCode.success

  private def calculate(seeds: Seq[Resource], maps: Seq[ResourceMap]): Long = {
    @tailrec
    def inner(resource: Resource): Resource = {
      maps.find(_.from == resource.`type`).flatMap(_.findDestinationResource(resource)) match {
        case _ if resource.`type` == DestinationEnum.Location => resource
        case Some(newResource) => inner(newResource)
        case None => resource
      }
    }

    seeds.map(inner).map(_.start).min
  }
}
