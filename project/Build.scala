import sbt._
import sbt.Keys._
import com.typesafe.sbt.GitPlugin.autoImport._
import com.typesafe.sbt.pgp.PgpKeys
import mdoc.MdocPlugin
import mdoc.MdocPlugin.autoImport.{mdoc => mdocTask, _}
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import pl.project13.scala.sbt.JmhPlugin
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, _}
import sbtrelease.ReleasePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import scalafix.sbt.ScalafixPlugin
import Lib._

object ScalaGraal {

  private val ghProject = "scala-graal"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    val BooPickle       = "1.3.3"
    val Cats            = "2.1.1"
    val Graal           = "20.2.0"
    val KindProjector   = "0.11.0"
    val Microlibs       = "2.3"
    val MonadicFor      = "0.3.1"
    val MTest           = "0.7.4"
    val Nyaya           = "0.9.2"
    val Prometheus      = "0.9.0"
    val Scala212        = "2.12.11"
    val Scala213        = "2.13.3"
    val ScalaCollCompat = "2.1.6"
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
    "-Ywarn-unused",
    "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.enablePlugins(ScalafixPlugin).settings(
      organization                  := "com.github.japgolly.scala-graal",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.MonadicFor),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector cross CrossVersion.full)))

  def testSettings = ConfigureBoth(
    _.settings(
      libraryDependencies ++= Seq(
        "com.lihaoyi"                   %%% "utest"     % Ver.MTest     % Test,
        "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test),
      testFrameworks := Seq(new TestFramework("utest.runner.Framework"))))
    .jsConfigure(
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val genExprBoilerplate = TaskKey[Unit]("genExprBoilerplate")

  lazy val genCacheAndReplaceBoilerplate = TaskKey[File]("genCacheAndReplaceBoilerplate")

  lazy val updateDoc = TaskKey[Unit]("updateDoc")

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings.jvm, preventPublication)
      .aggregate(
        core,
        coreJsJVM, coreJsJS,
        extBoopickleJVM, extBoopickleJS, extPrometheus,
        benchmark,
        mdoc)

  lazy val core = project
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettings.jvm)
    .settings(
      libraryDependencies ++= Seq(
        "org.graalvm.sdk"            % "graal-sdk"               % Ver.Graal,
        "org.scala-lang.modules"    %% "scala-collection-compat" % Ver.ScalaCollCompat,
        "org.typelevel"             %% "cats-core"               % Ver.Cats,
        "com.github.japgolly.nyaya" %% "nyaya-test"              % Ver.Nyaya % Test
      ),
      genExprBoilerplate := GenExprBoilerplate(sourceDirectory.value / "main"),
      genCacheAndReplaceBoilerplate := GenCacheAndReplaceBoilerplate(sourceDirectory.value / "main" / "scala"))

  lazy val coreJsJS  = coreJs.js
  lazy val coreJsJVM = coreJs.jvm
  lazy val coreJs = crossProject(JSPlatform, JVMPlatform)
    .in(file("core-js"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .settings(name := "core-js")
    .jvmConfigure(_
      .dependsOn(core)
      .settings(
        initialCommands := "import japgolly.scalagraal._, GraalJs._; val ctx = ContextSync()"))

  lazy val extBoopickle = crossProject(JSPlatform, JVMPlatform)
    .in(file("ext-boopickle"))
    .configureCross(commonSettings, publicationSettings, testSettings)
    .jvmConfigure(_.dependsOn(coreJsJVM))
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
    .dependsOn(core, coreJsJVM, extBoopickleJVM)
    .settings(unmanagedResources in Compile += (fullOptJS in Test in extBoopickleJS).value.data)

  lazy val mdoc = project
    .in(file(".mdoc"))
    .configure(commonSettings.jvm, preventPublication)
    .dependsOn(core, coreJsJVM, extBoopickleJVM, extPrometheus)
    .enablePlugins(MdocPlugin)
    .settings(
      mdocIn := baseDirectory.in(ThisBuild).value / "mdoc",
      mdocOut := baseDirectory.in(ThisBuild).value / "doc",
      mdocVariables := Map(
        "VERSION" -> advertiseVersion(version.value, git.gitDescribedVersion.value)
      ),
      updateDoc := {
        mdocTask.toTask("").value
        val readme = "README.md"
        val src = mdocOut.value / readme
        val tgt = baseDirectory.in(ThisBuild).value / readme
        IO.move(src, tgt)
      }
    )
}
