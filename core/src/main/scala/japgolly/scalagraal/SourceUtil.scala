package japgolly.scalagraal

import java.io.FileNotFoundException
import org.graalvm.polyglot.Source
import scala.io.Codec

object SourceUtil {

  def fileOnClasspath(filename: String)(implicit lang: Language, codec: Codec): Option[Source] =
    fileOnClasspath(lang.name, filename)(codec)

  def fileOnClasspath(lang: String, filename: String)(implicit codec: Codec): Option[Source] =
    Option(getClass.getClassLoader.getResourceAsStream(filename)).map { is =>
      val body = try scala.io.Source.fromInputStream(is)(codec).mkString finally is.close()
      Source.create(lang, body)
    }

  def requireFileOnClasspath(filename: String)(implicit lang: Language, codec: Codec): Source =
    requireFileOnClasspath(lang.name, filename)(codec)

  def requireFileOnClasspath(lang: String, filename: String)(implicit codec: Codec): Source =
    fileOnClasspath(lang, filename)
      .getOrElse(throw new FileNotFoundException(s"Classpath resource not found: $filename"))
}
