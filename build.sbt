lazy val root: Project =
  project.in(file("."))
    .configs(IntegrationTest extend Test)
    .settings(inConfig(IntegrationTest extend Test)(Defaults.testSettings): _*)
    .settings(Defaults.itSettings: _*)
    .settings(
      name := "scala-3-backwards",
      organization := "tech.backwards",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "3.2.1",
      fork := true,
      libraryDependencies ++= Dependencies()
    )