name := "scala-graal"

ThisBuild / homepage      := Some(url("https://github.com/japgolly/scala-graal"))
ThisBuild / licenses      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))
ThisBuild / organization  := "com.github.japgolly.scala-graal"
ThisBuild / shellPrompt   := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear     := Some(2018)
ThisBuild / versionScheme := Some("early-semver")

val root            = ScalaGraal.root
val core            = ScalaGraal.core
val coreJsJS        = ScalaGraal.coreJsJS
val coreJsJVM       = ScalaGraal.coreJsJVM
val extBoopickleJS  = ScalaGraal.extBoopickleJS
val extBoopickleJVM = ScalaGraal.extBoopickleJVM
val extPrometheus   = ScalaGraal.extPrometheus
val benchmark       = ScalaGraal.benchmark
val mdoc            = ScalaGraal.mdoc
