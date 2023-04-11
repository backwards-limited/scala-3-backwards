lazy val root: Project =
  project.in(file("."))
    .configs(IntegrationTest extend Test)
    .settings(inConfig(IntegrationTest extend Test)(Defaults.testSettings) *)
    .settings(Defaults.itSettings *)
    .settings(
      name := "scala-3-backwards",
      organization := "tech.backwards",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "3.2.2",
      fork := true,
      libraryDependencies ++= Dependencies()
    )