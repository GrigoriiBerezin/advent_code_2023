import Dependencies._

lazy val commonSettings = Def.settings(
  scalaVersion := "2.13.12",
  libraryDependencies := coreDependencies
)

lazy val common = project.settings(commonSettings)
lazy val task01 = project.settings(commonSettings).dependsOn(common)
lazy val task02 = project.settings(commonSettings).dependsOn(common)