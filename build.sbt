import Dependencies._

lazy val commonSettings = Def.settings(
  scalaVersion := "2.13.10",
  libraryDependencies := coreDependencies ++ testDependencies
)

lazy val pureScala3 = Def.settings(
  scalaVersion := "3.2.1",
  libraryDependencies := testDependencies
)

lazy val common = project.settings(commonSettings)
lazy val task01 = project.settings(commonSettings).dependsOn(common)
lazy val task02 = project.settings(commonSettings).dependsOn(common)
lazy val task03 = project.settings(commonSettings).dependsOn(common)
lazy val task04 = project.settings(commonSettings).dependsOn(common)
lazy val task05 = project.settings(commonSettings).dependsOn(common)
lazy val task05_scala3 = project.settings(pureScala3)
lazy val task06 = project.settings(commonSettings).dependsOn(common)
lazy val task07 = project.settings(commonSettings).dependsOn(common)
lazy val task08 = project.settings(commonSettings).dependsOn(common)
lazy val task09 = project.settings(commonSettings).dependsOn(common)
lazy val task10 = project.settings(commonSettings).dependsOn(common) // redo
lazy val task11 = project.settings(commonSettings).dependsOn(common)
lazy val task12 = project.settings(commonSettings).dependsOn(common)
lazy val task13 = project.settings(commonSettings).dependsOn(common)
lazy val task14 = project.settings(commonSettings).dependsOn(common) // refactor cycle movements
lazy val task15 = project.settings(commonSettings).dependsOn(common)