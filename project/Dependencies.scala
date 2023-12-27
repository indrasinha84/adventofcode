import Dependencies.Compile.scalaParallel
import Dependencies.Test.*
import sbt.*

object Dependencies {

  val testDependencies: Seq[ModuleID] = Seq(scalaTest)
  val mainDependencies: Seq[ModuleID] = Seq(scalaParallel)
  val dependencies = testDependencies ++ mainDependencies

  object Test {
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.17"
  }

  object Compile {
    val scalaParallel = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  }

}
