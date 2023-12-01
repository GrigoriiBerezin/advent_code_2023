import sbt.librarymanagement.ModuleID
import sbt.librarymanagement.syntax._

object Dependencies {
  object Version {
    lazy val zio = "2.0.19"
    lazy val cats = "2.10.0"
  }

  lazy val zioCore = "dev.zio" %% "zio" % Version.zio
  lazy val catsCore = "org.typelevel" %% "cats-core" % Version.cats

  lazy val coreDependencies: Seq[ModuleID] = Seq(zioCore, catsCore)
}
