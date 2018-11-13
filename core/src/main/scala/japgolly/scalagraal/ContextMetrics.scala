package japgolly.scalagraal

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

  object Println extends Writer {
    private val f: DurationLite => String = _.toStrUs
    override def apply(waited: DurationLite,
                       pre: DurationLite,
                       eval: DurationLite,
                       post: DurationLite,
                       total: DurationLite): Unit = {
      println(s"[eval] waited: ${f(waited)} | pre: ${f(pre)} | eval: ${f(eval)} | post: ${f(post)} | total: ${f(total)}")
    }
  }
}