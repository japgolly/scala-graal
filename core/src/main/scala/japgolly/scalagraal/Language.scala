package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Source, Value}

sealed trait Language {
  import Language.Binding

  val name: String
  def polyglotImport(b: Binding): String
  def bound(b: Binding): Source => Context => Value
  def translateValue(value: Any): Any

  private[scalagraal] val argBinding = Language.Binding("__scalagraal_arg")
  private[scalagraal] val argBinder = bound(argBinding)
  private[scalagraal] val argElement = (0 until 22).map(i => s"${argBinding.localValue}[$i]").toVector
}

object Language {

  final case class Binding(bindingName: String, localValue: String) {
    def set(ctx: Context, value: AnyRef): Unit =
      ctx.getPolyglotBindings.putMember(bindingName, value)

    def withValue[A](ctx: Context, value: AnyRef)(a: => A): A = {
      set(ctx, value)
      try a finally ctx.getPolyglotBindings.removeMember(bindingName)
    }
  }

  object Binding {
    def apply(name: String): Binding =
      apply(name, name)

    def apply(bindingName: String, localValue: String): Binding =
      new Binding(bindingName, localValue)
  }

  case object JS extends Language {
    override val name = "js"

    override def polyglotImport(b: Binding): String =
      s"Polyglot.import('${b.bindingName}')"

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
