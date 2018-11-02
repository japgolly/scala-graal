import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object Lib {

  def publicationSettings(ghProject: String): Project => Project =
    _.settings(
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      pomExtra :=
        <scm>
          <connection>scm:git:github.com/japgolly/{ghProject}</connection>
          <developerConnection>scm:git:git@github.com:japgolly/{ghProject}.git</developerConnection>
          <url>github.com:japgolly/{ghProject}.git</url>
        </scm>
        <developers>
          <developer>
            <id>japgolly</id>
            <name>David Barri</name>
          </developer>
        </developers>)

  def preventPublication: Project => Project =
    _.settings(
      skip in publish    := true,
      publishArtifact    := false,
      publishTo          := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      packagedArtifacts  := Map.empty)
    // .disablePlugins(plugins.IvyPlugin)

  def byScalaVersion[A](f: PartialFunction[(Long, Long), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(CrossVersion.partialVersion(scalaVersion.value).flatMap(f.lift).getOrElse(Nil))
}
