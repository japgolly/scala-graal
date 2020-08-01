package japgolly.scalagraal.js

import japgolly.scalagraal._

object LanguageJs extends Language("js") {

  override def polyglotImport(b: Binding) =
    s"Polyglot.import('${b.bindingName}')"

  override protected def setBinding(a: String, b: String) =
    a + "=" + b

  override protected def unsetBinding(a: String) =
    a + "=undefined"
}
