import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Exported
    val boopickle        = "1.4.0"
    val cats             = "2.8.0"
    val graal            = "21.3.1"
    val microlibs        = "4.2.1"
    val prometheus       = "0.16.0"
    val scala2           = "2.13.9"
    val scala3           = "3.1.3"

    // Internal
    val betterMonadicFor = "0.3.1"
    val kindProjector    = "0.13.2"
    val nyaya            = "1.1.0"
    val utest            = "0.8.1"
  }

  object Dep {
    val boopickle         = Def.setting("io.suzaku"                     %%% "boopickle"    % Ver.boopickle)
    val cats              = Def.setting("org.typelevel"                 %%% "cats-core"    % Ver.cats)
    val graalSdk          = Def.setting("org.graalvm.sdk"                 % "graal-sdk"    % Ver.graal)
    val microlibsTestUtil = Def.setting("com.github.japgolly.microlibs" %%% "test-util"    % Ver.microlibs)
    val nyayaTest         = Def.setting("com.github.japgolly.nyaya"     %%% "nyaya-test"   % Ver.nyaya)
    val prometheus        = Def.setting("io.prometheus"                   % "simpleclient" % Ver.prometheus)
    val utest             = Def.setting("com.lihaoyi"                   %%% "utest"        % Ver.utest)

    // Compiler plugins
    val betterMonadicFor = compilerPlugin("com.olegpy"     %% "better-monadic-for" % Ver.betterMonadicFor)
    val kindProjector    = compilerPlugin("org.typelevel"  %% "kind-projector"     % Ver.kindProjector cross CrossVersion.full)
  }
}
