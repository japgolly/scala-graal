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
    val BooPickle       = "1.3.1"
    val Cats            = "2.0.0-RC2"
    val Graal           = "19.2.1"
    val KindProjector   = "0.10.3"
    val Microlibs       = "2.0-RC1"
    val MonadicFor      = "0.3.1"
    val MTest           = "0.7.1"
    val Nyaya           = "0.9.0-RC1"
    val Prometheus      = "0.6.0"
    val Scala212        = "2.12.8"
    val Scala213        = "2.13.0"
    val ScalaCollCompat = "2.1.2"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:inline",
    "-opt-inline-from:japgolly.scalagraal.**",
    "-Ywarn-dead-code",
    // "-Ywarn-unused",
    "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.scala-graal",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage              := Watched.clearWhenTriggered,
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.MonadicFor),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector)))

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

  lazy val genCacheAndReplaceBoilerplate = TaskKey[File]("genCacheAndReplaceBoilerplate")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        core,
        utilJVM, utilJS,
        extBoopickleJVM, extBoopickleJS, extPrometheus,
        benchmark)

  lazy val core = project
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettings.jvm)
    .settings(
      libraryDependencies ++= Seq(
        "org.graalvm.sdk"         % "graal-sdk"               % Ver.Graal,
        "org.scala-lang.modules" %% "scala-collection-compat" % Ver.ScalaCollCompat
      ),
      initialCommands := "import japgolly.scalagraal._, GraalJs._; val ctx = ContextSync()",
      genExprBoilerplate := GenExprBoilerplate(sourceDirectory.value / "main" / "scala"))

  lazy val utilJS  = util.js
  lazy val utilJVM = util.jvm
  lazy val util = crossProject(JSPlatform, JVMPlatform)
    .in(file("util"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .jvmConfigure(_
      .dependsOn(core)
      .settings(
        libraryDependencies ++= Seq(
          "org.typelevel"             %% "cats-core"  % Ver.Cats,
          "com.github.japgolly.nyaya" %% "nyaya-test" % Ver.Nyaya % Test),
        genCacheAndReplaceBoilerplate := GenCacheAndReplaceBoilerplate(sourceDirectory.value / "main" / "scala")))

  lazy val extBoopickle = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-boopickle"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .jvmConfigure(_.dependsOn(core))
    .settings(
      name := "ext-boopickle",
      libraryDependencies += "io.suzaku" %%% "boopickle" % Ver.BooPickle)

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
    .dependsOn(core, utilJVM, extBoopickleJVM)
    .settings(unmanagedResources in Compile += (fullOptJS in Test in extBoopickleJS).value.data)
}
