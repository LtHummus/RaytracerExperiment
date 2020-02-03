name := "RaytracerExperiment"

version := "0.1"

scalaVersion := "2.13.1"

//test dependencies
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"


//logging
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

//parallel collections
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"