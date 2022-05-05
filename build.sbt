ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

val json4sNative = "org.json4s" %% "json4s-native" % "4.0.5"

lazy val root = (project in file("."))
  .settings(
    name := "scrambler",
    idePackagePrefix := Some("com.chrisworks"),
    libraryDependencies += json4sNative
  )
