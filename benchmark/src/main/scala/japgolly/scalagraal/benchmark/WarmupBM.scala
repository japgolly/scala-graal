package japgolly.scalagraal.benchmark

import japgolly.scalagraal.SourceUtil
import java.util.concurrent.TimeUnit
import org.graalvm.polyglot.{Context, Engine, Source}
import org.openjdk.jmh.annotations._

//[info] Benchmark        (warmupContexts)  (warmupRenders)  Mode  Cnt    Score     Error  Units
//[info] WarmupBM.render                 0             1000  avgt   40  186.561 ± 146.699  ms/op
//[info] WarmupBM.render                 1             1000  avgt   40   73.349 ±  47.918  ms/op
//[info] WarmupBM.render                10             1000  avgt   40   18.536 ±   5.427  ms/op
//[info] WarmupBM.render               100             1000  avgt   40   11.542 ±   2.710  ms/op
//[info] WarmupBM.render              1000             1000  avgt   40    9.835 ±   4.310  ms/op

//[info] Benchmark        (warmupContexts)  (warmupRenders)  Mode  Cnt    Score     Error  Units
//[info] WarmupBM.render                 0             2000  avgt   40  185.371 ± 143.554  ms/op
//[info] WarmupBM.render                 1             2000  avgt   40   71.920 ±  51.349  ms/op
//[info] WarmupBM.render                 2             2000  avgt   40   29.244 ±  15.615  ms/op
//[info] WarmupBM.render                10             2000  avgt   40   17.469 ±   6.050  ms/op
//[info] WarmupBM.render                20             2000  avgt   40   14.743 ±   4.794  ms/op
//[info] WarmupBM.render               100             2000  avgt   40   10.870 ±   2.405  ms/op
//[info] WarmupBM.render               200             2000  avgt   40    8.802 ±   1.900  ms/op
//[info] WarmupBM.render              1000             2000  avgt   40    7.102 ±   1.975  ms/op
//[info] WarmupBM.render              2000             2000  avgt   40    5.974 ±   1.012  ms/op

//[info] Benchmark        (warmupContexts)  (warmupRenders)  Mode  Cnt    Score     Error  Units
//[info] WarmupBM.render                 0            10000  avgt   40  191.544 ± 155.303  ms/op
//[info] WarmupBM.render                 1            10000  avgt   40   65.249 ±  39.473  ms/op
//[info] WarmupBM.render                10            10000  avgt   40   11.792 ±   3.300  ms/op
//[info] WarmupBM.render               100            10000  avgt   40    6.927 ±   2.271  ms/op
//[info] WarmupBM.render              1000            10000  avgt   40    4.401 ±   1.441  ms/op
//[info] WarmupBM.render             10000            10000  avgt   40    4.808 ±   1.080  ms/op

object WarmupBM {

  val Libs: List[Source] =
    List(
      SourceUtil.requireFileOnClasspath("js", "react.production.min.js"),
      SourceUtil.requireFileOnClasspath("js", "react-dom-server.browser.production.min.js"),
      Source.create("js", "window = {console: console, location: {href: 'https://shipreq.com'}, navigator: {userAgent: ''}}"),
      SourceUtil.requireFileOnClasspath("js", "sample-sjr-spa.js"))

  val Render = Source.create("js", "A.comp2()")

  def newCtx(engine: Engine) = {
    implicit val ctx = Context.newBuilder("js").engine(engine).build()
    ctx.enter()
    Libs.foreach(ctx.eval)
    ctx
  }

  def closeCtx(ctx: Context): Unit = {
    ctx.leave()
    ctx.close()
  }
}


@Warmup(iterations = 0)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.NANOSECONDS)
@Fork(4)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class WarmupBM {
  import WarmupBM._

  @Param(Array("0", "1", "2", "10", "20", "100", "200", "1000", "2000"))
  var warmupContexts: Int = _

  @Param(Array("2000"))
  var warmupRenders: Int = _

  var ctx: Context = _

  @Setup def setup: Unit = {
    WarmupBM
    val engine = Engine.newBuilder().build()
    if (warmupContexts > 0) {
      val renders = warmupRenders / warmupContexts
      for (_ <- 1 to warmupContexts) {
        val ctx = newCtx(engine)
        for (_ <- 1 to renders) {
          ctx.eval(Render)
        }
        closeCtx(ctx)
      }
    }
    this.ctx = newCtx(engine)
  }

  @Benchmark def render = ctx.eval(Render)
}
