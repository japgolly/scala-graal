package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Engine, PolyglotAccess}

private[scalagraal] object InternalUtils {

  def newContext(permittedLangs: Seq[String]): Context =
    newBuilder(permittedLangs).build()

  def newContext(permittedLangs: Seq[String], engine: Engine): Context =
    newBuilder(permittedLangs).engine(engine).build()

  private def newBuilder(permittedLangs: Seq[String]) =
    Context
      .newBuilder(permittedLangs: _*)
      .allowPolyglotAccess(PolyglotAccess.ALL) // So Polyglot.import works
      .allowAllAccess(true) // So methods on Java objects can be invoked, Java arrays are recognised as JS arrays, etc
}
