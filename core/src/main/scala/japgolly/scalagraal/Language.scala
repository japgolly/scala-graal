package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Source, Value}

sealed trait Language {

  val name: String
  def bound(b: Language.Binding): Source => Context => Value
  def translateValue(value: Any): Any

  private[scalagraal] val argBinding = Language.Binding("__scalagraal_arg")
  private[scalagraal] val argBinder = bound(argBinding)
  private[scalagraal] val argElement = (1 to 22).map(i => s"${argBinding.localValue}[$i]").toVector
}

object Language {

  final case class Binding(bindingName: String, localValue: String)

  object Binding {
    def apply(name: String): Binding =
      apply(name, name)

    def apply(bindingName: String, localValue: String): Binding =
      new Binding(bindingName, localValue)
  }

  case object JS extends Language {
    override val name = "js"

    override def bound(b: Binding): Source => Context => Value = {
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

    override def translateValue(value: Any): Any = value match {
      case Some(a) => translateValue(a)
      case None    => null
      case a       => a
    }
  }
}
