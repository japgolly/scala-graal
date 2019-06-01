import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys
import pl.project13.scala.sbt.JmhPlugin
import sbtrelease.ReleasePlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{crossProject => _, CrossType => _, _}
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import Lib._

object ScalaGraal {

  private val ghProject = "scala-graal"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val BooPickle     = "1.3.1"
    final val Graal         = "19.0.0"
    final val KindProjector = "0.9.10"
    final val Microlibs     = "1.20"
    final val MTest         = "0.6.7"
    final val Prometheus    = "0.6.0"
    final val Scala211      = "2.11.12"
    final val Scala212      = "2.12.8"
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

  val commonSettings = ConfigureBoth(
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
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector)))

  def testSettings = ConfigureBoth(
    _.settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "utest"     % Ver.MTest     % Test,
        "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test),
      testFrameworks += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val genExprBoilerplate = TaskKey[File]("genExprBoilerplate")

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        core,
        extBoopickleJVM, extBoopickleJS, extPrometheus,
        benchmark)

  lazy val core = project
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettings.jvm)
    .settings(
      libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % Ver.Graal,
      genExprBoilerplate := GenExprBoilerplate(sourceDirectory.value / "main" / "scala"))

  lazy val extBoopickle = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-boopickle"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .jvmConfigure(_.dependsOn(core))
    .settings(
      name := "ext-boopickle",
      libraryDependencies += "io.suzaku" %%% "boopickle" % Ver.BooPickle)
    .jsSettings(test := (())) // https://github.com/scala-js/scala-js/issues/3673

  lazy val extBoopickleJS  = extBoopickle.js
  lazy val extBoopickleJVM = extBoopickle.jvm
    .settings(unmanagedResources in Test += (fastOptJS in Test in extBoopickleJS).value.data)

  lazy val extPrometheus = project
    .in(file("ext-prometheus"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettings.jvm)
    .dependsOn(core)
    .settings(
      name := "ext-prometheus",
      libraryDependencies += "io.prometheus" % "simpleclient" % Ver.Prometheus)

  lazy val benchmark = project
    .configure(commonSettings.jvm, preventPublication)
    .enablePlugins(JmhPlugin)
    .dependsOn(core, extBoopickleJVM)
    .settings(unmanagedResources in Compile += (fullOptJS in Test in extBoopickleJS).value.data)
}
