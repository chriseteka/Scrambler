ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

val circeVersion = "0.14.1"
val circeDependencies = Seq(
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-optics"
).map(_ % circeVersion)

lazy val root = (project in file("."))
  .settings(
    name := "scrambler",
    idePackagePrefix := Some("com.chrisworks"),
    libraryDependencies ++= circeDependencies
  )
