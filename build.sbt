name := "RaytracerExperiment"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions := Seq("-Ymacro-annotations")

//test dependencies
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"


//logging
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

//parallel collections
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

//parsing scene files
libraryDependencies += "io.circe" %% "circe-yaml" % "0.12.0"
val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

//command line parsing
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"