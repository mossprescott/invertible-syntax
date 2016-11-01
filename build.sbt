import sbt._
import Keys._
import de.heikoseeberger.sbtheader.license.Apache2_0

lazy val root = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(Seq(
    headers := Map(
      "scala" -> Apache2_0("2015 - 2016", "Moss Prescott")),
    scalaVersion := "2.11.8",
    name := "invertible-syntax",
    initialCommands in console := "import invertible._",
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core" % "7.2.2",
      "com.chuusai" %% "shapeless"   % "2.3.0",
      "org.specs2"  %% "specs2-core" % "3.7.3" % "test"
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-language:higherKinds"
    )
  ))
