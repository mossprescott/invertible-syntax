import sbt._
import Keys._
import de.heikoseeberger.sbtheader.license.Apache2_0

lazy val root = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(Seq(
    headers := Map(
      "scala" -> Apache2_0("2014 - 2015", "SlamData Inc.")),
    scalaVersion := "2.11.8",
    name := "invertible-syntax",
    initialCommands in console := "import invertible._, Syntax._",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.2",
      "org.specs2" %% "specs2-core" % "3.7.3" % "test"
    )
  ))
