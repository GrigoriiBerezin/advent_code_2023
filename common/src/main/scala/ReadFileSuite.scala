import zio.{Task, ZIO}

import scala.io.{BufferedSource, Source}

trait ReadFileSuite {
  protected def getLines(fileName: String): Task[Seq[String]] = {
    def acquireReader(): Task[BufferedSource] = ZIO.attempt(Source.fromResource(fileName))
    def releaseReader(reader: BufferedSource) = ZIO.succeed(reader.close())
    def useReader(reader: BufferedSource) = ZIO.attempt(reader.getLines().toSeq)

    ZIO.acquireReleaseWith(acquireReader())(releaseReader)(useReader)
  }
}
