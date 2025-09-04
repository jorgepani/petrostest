ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.16"
ThisBuild / organization := "com.jorgepani"

// sbt-assembly
import sbtassembly.MergeStrategy

lazy val root = (project in file("."))
  .settings(
    name := "petrostest",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "co.fs2" %% "fs2-io" % "3.10.2",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),
    Compile / mainClass := Some("petroschallenge.Main")
  )

Compile / mainClass := Some("petroschallenge.Main")

assembly / mainClass := Some("petroschallenge.Main")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _ @_*) => MergeStrategy.discard
  case "module-info.class"         => MergeStrategy.discard
  case _                           => MergeStrategy.first
}
