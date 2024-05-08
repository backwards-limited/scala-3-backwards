import sbt.*

object Dependencies {
  def apply(): Seq[ModuleID] =
    List(
      specs2, munit, weaver, scalacheck, scalatestContainers, scribe, pprint,
      cats, catsEffect, refined, monocle, shapeless, parserCombinators,
      http4s, fs2, sttp, circe
    ).flatten

  lazy val specs2: Seq[ModuleID] = {
    val group = "org.specs2"
    val version = "5.3.2"

    List(
      "specs2-core", "specs2-scalacheck", "specs2-matcher-extra"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc()) ++ List(
      "specs2-cats"
    ).map(group %% _ % "4.20.2" % "test, integration" withSources() withJavadoc())
  }

  lazy val munit: Seq[ModuleID] = {
    val group = "org.scalameta"
    val version = "1.0.0-M10"

    List(
      "munit", "munit-scalacheck"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc() exclude("org.typelevel", "munit-cats-effect-2_2.13") exclude("org.typelevel", "munit-cats-effect-2")) ++ List(
      "org.typelevel" %% "discipline-munit" % "2.0.0-M3" % "test, integration"
    )
  }

  lazy val weaver: Seq[ModuleID] =
    List("com.disneystreaming" %% "weaver-cats" % "0.8.4" % "test, integration" withSources() withJavadoc())


  lazy val scalacheck: Seq[ModuleID] =
    List("org.scalacheck" %% "scalacheck" % "1.17.1" % "test, integration" withSources() withJavadoc())

  lazy val scalatestContainers: Seq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "0.41.0"

    List(
      "testcontainers-scala-munit", "testcontainers-scala-kafka", "testcontainers-scala-mysql", "testcontainers-scala-postgresql"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc())
  }

  lazy val scribe: Seq[ModuleID] =
    List("com.outr" %% "scribe" % "3.13.2" withSources() withJavadoc())

  lazy val pprint: Seq[ModuleID] =
    List("com.lihaoyi" %% "pprint" % "0.8.1")

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.10.0"

    List(
      "cats-core", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc()) ++ List(
      "cats-mtl"
    ).map(group %% _ % "1.3.1" withSources() withJavadoc())
  }

  lazy val catsEffect: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "3.5.2"

    List(
      "cats-effect"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.11.0"

    List(
      "refined", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "dev.optics"
    val version = "3.2.0"

    List(
      "monocle-core", "monocle-macro"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "monocle-law"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc())
  }

  lazy val shapeless: Seq[ModuleID] =
    List("org.typelevel" %% "shapeless3-deriving" % "3.4.0")

  lazy val parserCombinators: Seq[ModuleID] =
    List("org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0")

  lazy val http4s: Seq[ModuleID] = {
    val group = "org.http4s"
    val version = "1.0.0-M38"

    List(
      "http4s-core", "http4s-dsl", "http4s-circe", "http4s-client", "http4s-blaze-client", "http4s-server", "http4s-blaze-server"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "3.9.2"

    List(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val sttp: Seq[ModuleID] = {
    val group = "com.softwaremill.sttp.client3"
    val version = "3.9.0"

    List(
      "core", "circe", "async-http-client-backend-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val circe: Seq[ModuleID] = {
    val group = "io.circe"
    val version = "0.14.6"

    List(
      "circe-core", "circe-generic", "circe-parser", "circe-refined"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "circe-testing", "circe-literal"
    ).map(group %% _ % version % "test, integration" withSources() withJavadoc())
  }
}