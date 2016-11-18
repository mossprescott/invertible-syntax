import sbt._
import Keys._
import de.heikoseeberger.sbtheader.license.Apache2_0

val commonSettings = Seq(
  headers := Map(
    "scala" -> Apache2_0("2015 - 2016", "Moss Prescott")),
  scalaVersion := "2.12.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Yno-adapted-args",
    // "-Yno-imports",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ydelambdafy:method",
    "-Ypartial-unification",
    //"-Yliteral-types",
    "-Ywarn-unused-import"
  )
)

lazy val root = (project in file("."))
  .aggregate(core, examples)

lazy val core = project
  .settings(commonSettings)
  .settings(Seq(
    name := "invertible-syntax",
    initialCommands in console := "import invertible._, Syntax._",
    libraryDependencies ++= Seq(
      "org.scalaz"  %% "scalaz-core" % "7.2.7",
      "com.chuusai" %% "shapeless"   % "2.3.2",
      "org.specs2"  %% "specs2-core" % "3.8.6" % "test"
    )
  ))
  .enablePlugins(AutomateHeaderPlugin)

lazy val examples = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(Seq(
    name := "invertible-syntax-examples",
    libraryDependencies ++= Seq(
      // "org.scalaz"  %% "scalaz-core" % "7.2.7",
      // "com.chuusai" %% "shapeless"   % "2.3.2",
      // "com.slamdata" % "matryoshka-core_2.11" % "0.11.1",
      "org.specs2"    %% "specs2-core"   % "3.8.6" % "test",
      "org.typelevel" %% "scalaz-specs2" % "0.5.0" % "test"
    )
  ))
  .enablePlugins(AutomateHeaderPlugin)
