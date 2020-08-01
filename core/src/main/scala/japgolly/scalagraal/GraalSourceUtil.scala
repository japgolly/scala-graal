package japgolly.scalagraal

import java.io.FileNotFoundException
import org.graalvm.polyglot.Source

/** These methods are meant as additions to the static methods in [[Source]].
  * They're impure methods, and take language as a plain String.
  *
  * You shouldn't use these directly; instead the same methods are available on [[Expr]].
  */
object GraalSourceUtil {

  def fileOnClasspath(lang: String, filename: String): Option[Source] =
    Option(getClass.getClassLoader.getResource(filename)).map { url =>
      Source.newBuilder(lang, url).build()
    }

  def requireFileOnClasspath(lang: String, filename: String): Source =
    fileOnClasspath(lang, filename)
      .getOrElse(throw new FileNotFoundException(s"Classpath resource not found: $filename"))
}
