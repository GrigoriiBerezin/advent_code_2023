import Dependencies._

lazy val commonSettings = Def.settings(
  scalaVersion := "2.13.10",
  libraryDependencies := coreDependencies ++ testDependencies
)

lazy val common = project.settings(commonSettings)
lazy val task01 = project.settings(commonSettings).dependsOn(common)
lazy val task02 = project.settings(commonSettings).dependsOn(common)
lazy val task03 = project.settings(commonSettings).dependsOn(common)
lazy val task04 = project.settings(commonSettings).dependsOn(common)
lazy val task05 = project.settings(commonSettings).dependsOn(common)