package japgolly.scalagraal

import japgolly.scalagraal.GraalContextMetrics._
import japgolly.scalagraal.util.DurationLite
import java.io.PrintStream

final class GraalContextMetrics(private val data: Array[Long]) extends AnyVal {
  override def toString = s"ContextMetrics($total)"

  private def get(i: Int): DurationLite = new DurationLite(data(i))
  def apply(m: Metric)   : DurationLite = get(m.ord)
  def waited             : DurationLite = get(0)
  def pre                : DurationLite = get(1)
  def body               : DurationLite = get(2)
  def post               : DurationLite = get(3)
  def total              : DurationLite = get(4)
}

object GraalContextMetrics {

  // Lots of yuk in this file to be fast as possible.

  def apply(waited: DurationLite,
            pre   : DurationLite,
            body  : DurationLite,
            post  : DurationLite,
            total : DurationLite): GraalContextMetrics = {
    val a = Array[Long](
      waited.nanos,
      pre   .nanos,
      body  .nanos,
      post  .nanos,
      total .nanos)
    new GraalContextMetrics(a)
  }

  val Zero: GraalContextMetrics = {
    val z = DurationLite.Zero
    apply(z, z, z, z, z)
  }

  final case class AndExprResult[+A](metrics: GraalContextMetrics, result: Expr.Result[A]) {
    override def toString = s"ContextMetrics.AndResult($metrics, $result)"
  }

  // ===================================================================================================================

  sealed abstract class Metric(final val ord: Int)
  object Metric {
    case object Wait  extends Metric(0)
    case object Pre   extends Metric(1)
    case object Body  extends Metric(2)
    case object Post  extends Metric(3)
    case object Total extends Metric(4)

    val values: Vector[Metric] =
      Vector(Wait, Pre, Body, Post, Total)

    def memo[A](f: Metric => A): Metric => A = {
      val m: Map[Metric, A] = values.iterator.map(m => (m, f(m))).toMap
      m.apply
    }
  }

  // ===================================================================================================================

  trait Writer { self =>
    def apply(m: GraalContextMetrics): Unit

    def >>(next: Writer): Writer =
      if (next eq Writer.Noop)
        this
      else
        new Writer {
          override def apply(m: GraalContextMetrics): Unit = {
            self(m)
            next(m)
          }
        }
  }

  object Writer {
    def perMetric(f: Metric => DurationLite => Unit): Writer = {
      val fWait  = f(Metric.Wait)
      val fPre   = f(Metric.Pre)
      val fBody  = f(Metric.Body)
      val fPost  = f(Metric.Post)
      val fTotal = f(Metric.Total)
      new Writer {
        override def apply(m: GraalContextMetrics): Unit = {
          fWait (m.waited)
          fPre  (m.pre)
          fBody (m.body)
          fPost (m.post)
          fTotal(m.total)
        }
      }
    }

    def apply(f: GraalContextMetrics => Unit): Writer =
      new Writer {
        override def apply(m: GraalContextMetrics): Unit = f(m)
      }

    object Noop extends Writer {
      override def apply(stats: GraalContextMetrics) = ()
      override def >>(next: Writer) = next
    }

    final case class Print(fmt : DurationLite => String = _.toStrMs,
                           name: String                 = "graal-eval",
                           to  : PrintStream            = System.out) extends Writer {

      override def apply(stats: GraalContextMetrics): Unit = {
        import stats._
        to.println(s"[$name] waited: ${fmt(waited)} | pre: ${fmt(pre)} | eval: ${fmt(body)} | post: ${fmt(post)} | total: ${fmt(total)}")
      }
    }

    final case class StoreLast() extends Writer {
      var last = GraalContextMetrics.Zero
      override def apply(stats: GraalContextMetrics): Unit =
        this.last = stats
    }
  }

}
