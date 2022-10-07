package japgolly.scalagraal

import japgolly.scalagraal.util.FileRef
import java.io.FileNotFoundException
import org.graalvm.polyglot.Source

/** These methods are meant as additions to the static methods in [[Source]].
  * They're impure methods, and take language as a plain String.
  *
  * You shouldn't use these directly; instead the same methods are available on [[Expr]].
  */
object GraalSourceUtil {

  def file[A](lang: String, file: A, srcCfg: Source#Builder => Source#Builder = identity)(implicit A: FileRef[A]): Option[Source] = {
    val f = A.file(file)
    Option.when(f.exists()) {
      val b = Source.newBuilder(lang, f)
      srcCfg(b).build()
    }
  }

  def requireFile[A](lang: String, file: A, srcCfg: Source#Builder => Source#Builder = identity)(implicit A: FileRef[A]): Source =
    this.file(lang, file, srcCfg)
      .getOrElse(throw new FileNotFoundException(s"file not found: ${A.str(file)}"))

  // ===================================================================================================================

  def fileOnClasspath(lang: String, filename: String, srcCfg: Source#Builder => Source#Builder = identity): Option[Source] =
    Option(getClass.getClassLoader.getResource(filename)).map { url =>
      val b = Source.newBuilder(lang, url)
      srcCfg(b).build()
    }

  def requireFileOnClasspath(lang: String, filename: String, srcCfg: Source#Builder => Source#Builder = identity): Source =
    fileOnClasspath(lang, filename, srcCfg)
      .getOrElse(throw new FileNotFoundException(s"Classpath resource not found: $filename"))
}
