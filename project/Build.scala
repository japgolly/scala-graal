import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys
import pl.project13.scala.sbt.JmhPlugin
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object ScalaGraal {

  private val ghProject = "scala-graal"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val Graal         = "1.0.0-rc8"
    final val KindProjector = "0.9.8"
    final val Microlibs     = "1.18"
    final val MTest         = "0.6.6"
    final val Scala211      = "2.11.12"
    final val Scala212      = "2.12.7"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    // "-Ywarn-unused",
    "-Ywarn-value-discard",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials")

  val commonSettings: Project => Project =
    _.settings(
      organization                  := "com.github.japgolly.scala-graal",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala212,
      crossScalaVersions            := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Compile     ++= byScalaVersion { case (2, 12) => Seq("-opt:l:method") }.value,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage              := Watched.clearWhenTriggered,
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector))

  def testSettings: Project => Project =
    _.settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %% "utest"     % Ver.MTest     % Test,
        "com.github.japgolly.microlibs" %% "test-util" % Ver.Microlibs % Test),
      testFrameworks += new TestFramework("utest.runner.Framework"))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(core, benchmark)

  lazy val core = project
    .configure(commonSettings, publicationSettings, testSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.graalvm.sdk" % "graal-sdk" % Ver.Graal))

  lazy val benchmark = project
    .configure(commonSettings, preventPublication)
    .enablePlugins(JmhPlugin)
    .dependsOn(core)
}
