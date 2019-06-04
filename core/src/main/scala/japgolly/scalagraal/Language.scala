package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Source, Value}

sealed abstract class Language(final val name: String) { self =>

  def polyglotImport(b: Binding): String

  final object Binding {
    def apply(name: String): Binding =
      apply(name, name)

    def apply(bindingName: String, localValue: String): Binding =
      new Binding(bindingName, localValue)
  }

  final class Binding(val bindingName: String, val localValue: String) {
    override def toString = {
      val a = if (bindingName == localValue) localValue else s"$bindingName, $localValue"
      s"$self.Binding($a)"
    }

    private[scalagraal] val set   = Source.create(name, setBinding(localValue, polyglotImport(this)))
    private[scalagraal] val unset = Source.create(name, unsetBinding(localValue))

    def set(ctx: Context, value: AnyRef): Unit =
      ctx.getPolyglotBindings.putMember(bindingName, value)

    def withValue[A](ctx: Context, value: AnyRef)(a: => A): A = {
      set(ctx, value)
      try
        a
      finally {
        ctx.getPolyglotBindings.removeMember(bindingName)
        ()
      }
    }
  }

  protected def setBinding(varName: String, value: String): String
  protected def unsetBinding(varName: String): String

  final def bound(b: Binding): Source => Context => Value =
    body => ctx => {
      ctx.eval(b.set)
      try
        ctx.eval(body)
      finally {
        ctx.eval(b.unset)
        ()
      }
    }

  private[scalagraal] val argBinding = Binding("ScalaGraalArg")
  private[scalagraal] val argBinder = bound(argBinding)
  private[scalagraal] val argElement = (0 until 22).map(i => s"${argBinding.localValue}[$i]").toArray.apply _
}

object Language {

  type JS = JS.type
  case object JS extends Language("js") {
    override def polyglotImport(b: Binding)                 = s"Polyglot.import('${b.bindingName}')"
    override protected def setBinding(a: String, b: String) = a + "=" + b
    override protected def unsetBinding(a: String)          = a + "=null"
  }
}
