lazy val IntegrationTestConfig: Configuration =
  config("integration") extend Test

lazy val integrationTestSettings: Seq[Def.Setting[?]] =
  inConfig(IntegrationTestConfig)(Defaults.testSettings)

lazy val root: Project =
  project.in(file("."))
    .configs(IntegrationTestConfig)
    .settings(
      name := "scala-3-backwards",
      organization := "tech.backwards",
      version := "0.1.0-SNAPSHOT",
      scalaVersion := "3.3.1",
      scalacOptions ++= List(
        "-explain",
        "-Yexplicit-nulls",
        "-Ykind-projector",
        "-Ysafe-init"
      ),
      fork := true,
      libraryDependencies ++= Dependencies(),
      integrationTestSettings
    )