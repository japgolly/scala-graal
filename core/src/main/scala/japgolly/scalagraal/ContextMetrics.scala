package japgolly.scalagraal

import java.io.PrintStream

object ContextMetrics {

  trait Writer { self =>
    def apply(stats: Stats): Unit

    def >>(next: Writer): Writer =
      if (next eq Writer.Noop)
        this
      else
        new Writer {
          override def apply(stats: Stats): Unit = {
            self(stats)
            next(stats)
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
        override def apply(stats: Stats): Unit = {
          fWait (stats.waited)
          fPre  (stats.pre)
          fBody (stats.body)
          fPost (stats.post)
          fTotal(stats.total)
        }
      }
    }

    def apply(f: Stats => Unit): Writer =
      new Writer {
        override def apply(stats: Stats): Unit = f(stats)
      }

    object Noop extends Writer {
      override def apply(stats: Stats) = ()
      override def >>(next: Writer) = next
    }

    final case class Print(fmt : DurationLite => String = _.toStrMs,
                           name: String                 = "graal-eval",
                           to  : PrintStream            = System.out) extends Writer {

      override def apply(stats: Stats): Unit = {
        import stats._
        to.println(s"[$name] waited: ${fmt(waited)} | pre: ${fmt(pre)} | eval: ${fmt(body)} | post: ${fmt(post)} | total: ${fmt(total)}")
      }
    }
  }

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
      val m: Map[Metric, A] = values.map(m => (m, f(m)))(collection.breakOut)
      m.apply
    }
  }

  final class Stats(private val data: Array[Long]) extends AnyVal {
    private def get(i: Int): DurationLite = new DurationLite(data(i))
    def apply(m: Metric)   : DurationLite = get(m.ord)
    def waited             : DurationLite = get(0)
    def pre                : DurationLite = get(1)
    def body               : DurationLite = get(2)
    def post               : DurationLite = get(3)
    def total              : DurationLite = get(4)
  }

  object Stats {
    def apply(waited: DurationLite,
              pre   : DurationLite,
              body  : DurationLite,
              post  : DurationLite,
              total : DurationLite): Stats = {
      val a = Array[Long](
        waited.nanos,
        pre   .nanos,
        body  .nanos,
        post  .nanos,
        total .nanos)
      new Stats(a)
    }

    val Zero: Stats = {
      val z = DurationLite.Zero
      apply(z, z, z, z, z)
    }
  }
}

// TODO Lots of yuk in this file to be fast as possible. Benchmark without some of the yuk, it might not be necessary.