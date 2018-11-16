package japgolly.scalagraal

import java.io.FileNotFoundException
import org.graalvm.polyglot.Source

object SourceUtil {

  def fileOnClasspath(filename: String)(implicit lang: Language): Option[Source] =
    fileOnClasspath(lang.name, filename)

  def fileOnClasspath(lang: String, filename: String): Option[Source] =
    Option(getClass.getClassLoader.getResource(filename)).map { url =>
      Source.newBuilder(lang, url).build()
    }

  def requireFileOnClasspath(filename: String)(implicit lang: Language): Source =
    requireFileOnClasspath(lang.name, filename)

  def requireFileOnClasspath(lang: String, filename: String): Source =
    fileOnClasspath(lang, filename)
      .getOrElse(throw new FileNotFoundException(s"Classpath resource not found: $filename"))
}
