package japgolly.scalagraal

import io.prometheus.client.{CollectorRegistry, Histogram}
import japgolly.scalagraal.ContextMetrics.Metric

/** Example:
  *
  * {{{
  *   GraalPrometheus.Builder()
  *     .configureAll(_.addLabel("name", "example"))
  *     .registerAndBuild()
  * }}}
  */
object GraalPrometheus {

  object Builder {
    def apply(): Builder = {
      import Default._
      new Builder(m => new RestrictedHistogramBuilder(
        Histogram.build(Name(m), Help(m)),
        Vector.empty))
    }
  }

  final class Builder(bh: Metric => RestrictedHistogramBuilder) {
    def configureAll(f: RestrictedHistogramBuilder => RestrictedHistogramBuilder): Builder =
      new Builder(f compose bh)

    def configure(m: Metric, f: RestrictedHistogramBuilder => RestrictedHistogramBuilder): Builder =
      new Builder(i => if (i == m) f(bh(m)) else bh(i))

    def registerAndBuild(): ContextMetrics.Writer =
      build(_.register())

    def registerAndBuild(registry: CollectorRegistry): ContextMetrics.Writer =
      build(_.register(registry))

    private def build(register: Histogram.Builder => Histogram): ContextMetrics.Writer =
      ContextMetrics.Writer { m =>
        val r = bh(m)
        val h = register(r.b.labelNames(r.l.map(_._1): _*))
        if (r.l.isEmpty)
          dur => h.observe(dur.seconds)
        else {
          val h2 = h.labels(r.l.map(_._2): _*)
          dur => h2.observe(dur.seconds)
        }
      }
  }

  final class RestrictedHistogramBuilder(private[GraalPrometheus] val b: Histogram.Builder,
                                         private[GraalPrometheus] val l: Vector[(String, String)]) {

    private[GraalPrometheus] def mod(f: Histogram.Builder => Histogram.Builder): RestrictedHistogramBuilder =
      new RestrictedHistogramBuilder(f(b), l)

    def addLabel(name: String, value: String): RestrictedHistogramBuilder =
      new RestrictedHistogramBuilder(b, l :+ ((name, value)))

    def buckets(buckets: Double*): RestrictedHistogramBuilder =
      mod(_.buckets(buckets: _*))

    def exponentialBuckets(start: Double, factor: Double, count: Int): RestrictedHistogramBuilder =
      mod(_.exponentialBuckets(start, factor, count))

    def linearBuckets(start: Double, width: Double, count: Int): RestrictedHistogramBuilder =
      mod(_.linearBuckets(start, width, count))

    def namespace(namespace: String): RestrictedHistogramBuilder =
      mod(_.namespace(namespace))

    def subsystem(subsystem: String): RestrictedHistogramBuilder =
      mod(_.subsystem(subsystem))
  }

  private[scalagraal] object Default {

    val NamePrefix = "scalagraal_context_eval_"

    val NameSuffixes: Metric => String = {
      case Metric.Wait  => "wait_seconds"
      case Metric.Pre   => "pre_seconds"
      case Metric.Body  => "body_seconds"
      case Metric.Post  => "post_seconds"
      case Metric.Total => "total_seconds"
    }

    val Name: Metric => String =
      Metric.memo(NamePrefix + NameSuffixes(_))

    val Help: Metric => String = {
      case Metric.Wait  => "Time in seconds for a task to begin after submission."
      case Metric.Pre   => "Time in seconds to prepare a Context before each evaluation."
      case Metric.Body  => "Time in seconds to execute each submitted expression."
      case Metric.Post  => "Time in seconds to release a Context after each evaluation."
      case Metric.Total => "Total time in seconds between task submission and completion."
    }
  }
}