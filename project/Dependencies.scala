import Dependencies.Test.*
import sbt.*

object Dependencies {

  val testDependencies: Seq[ModuleID] = Seq(scalaTest).map(_ % "test, it")
  val mainDependencies: Seq[ModuleID] = Seq.empty
  val dependencies = testDependencies ++ mainDependencies

  object Test {
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.17"
  }

  object Compile {

  }

}
