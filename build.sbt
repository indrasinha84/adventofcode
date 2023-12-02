ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode"
  )
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .aggregate(Day1, Day2, Day3)


lazy val Day1 =project
  .settings (publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )

lazy val Day2 =project
  .settings (publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )

lazy val Day3 =project
  .settings (publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )