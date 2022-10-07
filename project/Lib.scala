import sbt._
import sbt.Keys._
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._

object Lib {

  type CPE = CrossProject => CrossProject
  type PE = Project => Project

  class ConfigureBoth(val jvm: PE, val js: PE) {
    def jvmConfigure(f: PE) = new ConfigureBoth(f compose jvm, js)
    def  jsConfigure(f: PE) = new ConfigureBoth(jvm, f compose js)
  }

  def ConfigureBoth(both: PE) = new ConfigureBoth(both, both)

  implicit def _configureBothToCPE(p: ConfigureBoth): CPE =
    _.jvmConfigure(p.jvm).jsConfigure(p.js)

  implicit class CrossProjectExt(val cp: CrossProject) extends AnyVal {
    def bothConfigure(fs: PE*): CrossProject =
      fs.foldLeft(cp)((q, f) =>
        q.jvmConfigure(f).jsConfigure(f))
  }
  implicit def CrossProjectExtB(b: CrossProject.Builder) =
    new CrossProjectExt(b)

  def publicationSettings(ghProject: String) =
    ConfigureBoth(
      _.settings(
        developers := List(
          Developer("japgolly", "David Barri", "japgolly@gmail.com", url("https://japgolly.github.io/japgolly/")),
        ),
      )
    )
      .jsConfigure(
        sourceMapsToGithub(ghProject))

  def sourceMapsToGithub(ghProject: String): Project => Project =
    p => p.settings(
      scalacOptions ++= {
        val isDotty = scalaVersion.value startsWith "3"
        val ver     = version.value
        if (isSnapshot.value)
          Nil
        else {
          val a = p.base.toURI.toString.replaceFirst("[^/]+/?$", "")
          val g = s"https://raw.githubusercontent.com/japgolly/$ghProject"
          val flag = if (isDotty) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
          s"$flag:$a->$g/v$ver/" :: Nil
        }
      }
    )

  def preventPublication: Project => Project =
    _.settings(publish / skip := true)

  def byScalaVersion[A](f: PartialFunction[(Long, Long), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(CrossVersion.partialVersion(scalaVersion.value).flatMap(f.lift).getOrElse(Nil))

  def advertiseVersion(currentVer: String, gitVer: Option[String]) =
    if (!currentVer.contains("SNAPSHOT"))
      currentVer
    else
      gitVer.fold(currentVer)(_.takeWhile(_ != '-'))
}
