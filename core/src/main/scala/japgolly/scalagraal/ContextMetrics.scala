package japgolly.scalagraal

import java.io.PrintStream

object ContextMetrics {

  final case class Dimension(name: String, value: Expr.Result[Any] => String)

  trait Writer { self =>
    def apply(waited: DurationLite,
              pre: DurationLite,
              eval: DurationLite,
              post: DurationLite,
              total: DurationLite): Unit

    def >>(next: Writer): Writer =
      if (next eq Noop)
        this
      else
        new Writer {
          override def apply(waited: DurationLite,
                             pre: DurationLite,
                             eval: DurationLite,
                             post: DurationLite,
                             total: DurationLite): Unit = {
            self(
              waited = waited,
              pre = pre,
              eval = eval,
              post = post,
              total = total)
            next(
              waited = waited,
              pre = pre,
              eval = eval,
              post = post,
              total = total)
          }
        }
  }

  object Noop extends Writer {
    override def apply(a: DurationLite, b: DurationLite, c: DurationLite, d: DurationLite, e: DurationLite) = ()
    override def >>(next: Writer) = next
  }

  final case class Print(fmt : DurationLite => String = _.toStrMs,
                         name: String                 = "graal-eval",
                         to  : PrintStream            = System.out) extends Writer {

    override def apply(waited: DurationLite,
                       pre: DurationLite,
                       eval: DurationLite,
                       post: DurationLite,
                       total: DurationLite): Unit =
      to.println(s"[$name] waited: ${fmt(waited)} | pre: ${fmt(pre)} | eval: ${fmt(eval)} | post: ${fmt(post)} | total: ${fmt(total)}")
  }
}