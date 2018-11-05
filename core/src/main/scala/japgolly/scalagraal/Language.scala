package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Source, Value}

final case class Language(name: String,
                          bound: Language.Binding => Source => Context => Value) {

  private[scalagraal] val scalaGraalArgB = Language.Binding("__scalagraal_arg")
  private[scalagraal] val scalaGraalArgF = bound(scalaGraalArgB)
}

object Language {

  final case class Binding(bindingName: String, localValue: String)
  object Binding {
    def apply(name: String): Binding =
      apply(name, name)

    def apply(bindingName: String, localValue: String): Binding =
      new Binding(bindingName, localValue)
  }

  val JS = Language(
    "js",
    b => {
      val set   = Source.create("js", s"${b.localValue}=Polyglot.import('${b.bindingName}')")
      val unset = Source.create("js", s"${b.localValue}=null")
      body => ctx => {
        ctx.eval(set)
        try
          ctx.eval(body)
        finally {
          ctx.eval(unset)
          ()
        }
      }
    }
  )
}
