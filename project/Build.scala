import sbt._
import sbt.Keys._
import com.typesafe.sbt.GitPlugin.autoImport._
import com.jsuereth.sbtpgp.PgpKeys
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
import Dependencies._

object ScalaGraal {

  private val ghProject = "scala-graal"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  def scalacCommonFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
  )

  def scalac2Flags = Seq(
    "-opt:l:inline",
    "-opt-inline-from:japgolly.scalagraal.**",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
  )

  def scalac3Flags = Seq(
    "-source", "3.0-migration",
    "-Ykind-projector",
  )

  val commonSettings = ConfigureBoth(
    _.enablePlugins(ScalafixPlugin).settings(
      organization                  := "com.github.japgolly.scala-graal",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.scala2,
      crossScalaVersions            := Seq(Ver.scala2, Ver.scala3),
      scalacOptions                ++= scalacCommonFlags,
      scalacOptions                ++= scalac2Flags.filter(_ => scalaVersion.value.startsWith("2")),
      scalacOptions                ++= scalac3Flags.filter(_ => scalaVersion.value.startsWith("3")),
      Test / scalacOptions         --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Nil,
      ThisBuild / shellPrompt       := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
      libraryDependencies          ++= Seq(Dep.betterMonadicFor, Dep.kindProjector).filter(_ => scalaVersion.value startsWith "2"),
  ))

  def testSettings = ConfigureBoth(
    _.settings(
      libraryDependencies ++= Seq(
        Dep.utest            .value % Test,
        Dep.microlibsTestUtil.value % Test,
      ),
      testFrameworks := Seq(new TestFramework("utest.runner.Framework"))))
    .jsConfigure(
      _.settings(Test / jsEnv := new JSDOMNodeJSEnv))

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  lazy val genExprBoilerplate = TaskKey[Unit]("genExprBoilerplate")

  lazy val genCacheAndReplaceBoilerplate = TaskKey[File]("genCacheAndReplaceBoilerplate")

  lazy val genStrFnCacheParamBoilerplate = TaskKey[File]("genStrFnCacheParamBoilerplate")

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
        Dep.cats     .value,
        Dep.graalSdk .value,
        Dep.nyayaTest.value % Test
      ),
      genExprBoilerplate := GenExprBoilerplate(sourceDirectory.value / "main"),
      genCacheAndReplaceBoilerplate := GenCacheAndReplaceBoilerplate(sourceDirectory.value / "main" / "scala"),
      genStrFnCacheParamBoilerplate := GenStrFnCacheParamBoilerplate(sourceDirectory.value / "main" / "scala"),
    )

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
      libraryDependencies += Dep.boopickle.value,
    )

  lazy val extBoopickleJS  = extBoopickle.js
  lazy val extBoopickleJVM = extBoopickle.jvm
    .settings(Test / unmanagedResources += (extBoopickleJS / Test / fastOptJS).value.data)

  lazy val extPrometheus = project
    .in(file("ext-prometheus"))
    .configure(commonSettings.jvm, publicationSettings.jvm, testSettings.jvm)
    .dependsOn(core)
    .settings(
      name := "ext-prometheus",
      libraryDependencies += Dep.prometheus.value,
    )

  lazy val benchmark = project
    .configure(commonSettings.jvm, preventPublication)
    .enablePlugins(JmhPlugin)
    .dependsOn(core, coreJsJVM, extBoopickleJVM)
    .settings(Compile / unmanagedResources += (extBoopickleJS / Test / fullOptJS).value.data)

  lazy val mdoc = project
    .in(file(".mdoc"))
    .configure(commonSettings.jvm, preventPublication)
    .dependsOn(core, coreJsJVM, extBoopickleJVM, extPrometheus)
    .enablePlugins(MdocPlugin)
    .settings(
      mdocIn := (ThisBuild / baseDirectory).value / "mdoc",
      mdocOut := (ThisBuild / baseDirectory).value / "doc",
      mdocVariables := Map(
        "VERSION" -> advertiseVersion(version.value, git.gitDescribedVersion.value)
      ),
      updateDoc := {
        mdocTask.toTask("").value
        val readme = "README.md"
        val src = mdocOut.value / readme
        val tgt = (ThisBuild / baseDirectory).value / readme
        IO.move(src, tgt)
      }
    )
}
