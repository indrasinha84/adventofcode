ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode"
  )
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .aggregate(common, Day1, Day2, Day3, Day4, Day5, Day6, Day7, Day8, Day9, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25)

lazy val common = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )

lazy val Day1 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day2 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day3 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)


lazy val Day4 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day5 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )

lazy val Day6 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day7 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day8 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day9 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day10 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day11 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day12 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day13 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day14 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day15 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day16 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day17 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day18 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day19 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day20 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day21 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day22 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day23 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day24 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)

lazy val Day25 = project
  .settings(publish := {})
  .settings(
    libraryDependencies ++= Dependencies.dependencies
  )
  .dependsOn(common)
