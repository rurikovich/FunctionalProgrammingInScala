name := "FunctionalProgrammingInScala"

version := "0.1"

scalaVersion := "2.13.3"

val scalaTestVersion = "3.2.3"
libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % Test


libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.1" % Test

// https://mvnrepository.com/artifact/org.scalatestplus/scalacheck-1-15
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0"
