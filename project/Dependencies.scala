import sbt.librarymanagement.ModuleID
import sbt.librarymanagement.syntax._

object Dependencies {
  object Version {
    lazy val zio = "2.0.19"
    lazy val cats = "2.10.0"
    lazy val scalatest = "3.2.17"
  }

  lazy val zioCore = "dev.zio" %% "zio" % Version.zio
  lazy val catsCore = "org.typelevel" %% "cats-core" % Version.cats

  lazy val scalatest = "org.scalatest" %% "scalatest" % Version.scalatest


  lazy val coreDependencies: Seq[ModuleID] = Seq(zioCore, catsCore)
  lazy val testDependencies: Seq[ModuleID] = Seq(scalatest).map(_ % Test)
}
