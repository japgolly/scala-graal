package japgolly.scalagraal

import io.prometheus.client.CollectorRegistry
import utest._
import GraalPrometheus.Default._
import japgolly.scalagraal.ContextMetrics.Metric

object GraalPrometheusTest extends TestSuite {

  def sec(s: Long) = {
    import scala.concurrent.duration._
    new DurationLite(s.seconds.toNanos)
  }

  //    import scala.collection.JavaConverters._
  //    registry.metricFamilySamples().asScala.toList.flatMap(_.samples.asScala.toList).foreach(a => println(a + ""))

  override def tests = Tests {

    'minimal {
      val registry = new CollectorRegistry

      val writer = GraalPrometheus.Builder()
        .registerAndBuild(registry)

      writer(ContextMetrics(
        waited = sec(1),
        pre = sec(3),
        body = sec(5),
        post = sec(2),
        total = sec(9)))

      def assertSample(expect: Double, m: Metric): Unit = {
        val sample = registry.getSampleValue(Name(m) + "_sum")
        assert(sample == expect)
      }

      'wait - assertSample(1, Metric.Wait)
      'pre - assertSample(3, Metric.Pre)
      'body - assertSample(5, Metric.Body)
      'post - assertSample(2, Metric.Post)
      'total - assertSample(9, Metric.Total)
    }

    'custom {
      val registry = new CollectorRegistry

      val writer = GraalPrometheus.Builder()
        .configure(_.addLabel("all", "AHH"))
        .configureByMetric{ case ContextMetrics.Metric.Body  => _.addLabel("body", "yep") }
        .configureByMetric{ case ContextMetrics.Metric.Total => _.addLabel("T", "tt") }
        .configureByMetric{ case ContextMetrics.Metric.Body  => _.addLabel("body2", "such amazing") }
        .registerAndBuild(registry)

      writer(ContextMetrics(
        waited = sec(1),
        pre = sec(3),
        body = sec(5),
        post = sec(2),
        total = sec(9)))

      def assertSample(expect: Double, m: Metric, ls: (String, String)*): Unit = {
        val sample = registry.getSampleValue(
          Name(m) + "_sum",
          Array("all" +: ls.map(_._1): _*),
          Array("AHH" +: ls.map(_._2): _*))
        assert(sample == expect)
      }

      'wait - assertSample(1, Metric.Wait)
      'pre - assertSample(3, Metric.Pre)
      'body - assertSample(5, Metric.Body, "body" -> "yep", "body2" -> "such amazing")
      'post - assertSample(2, Metric.Post)
      'total - assertSample(9, Metric.Total, "T" -> "tt")
    }

  }
}
