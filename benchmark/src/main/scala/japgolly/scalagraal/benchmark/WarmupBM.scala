package japgolly.scalagraal.benchmark

import japgolly.scalagraal.SourceUtil
import java.util.concurrent.TimeUnit
import org.graalvm.polyglot.{Context, Engine, Source}
import org.openjdk.jmh.annotations._

object WarmupBM {

  val Libs: List[Source] =
    List(
      SourceUtil.requireFileOnClasspath("js", "react.production.min.js"),
      SourceUtil.requireFileOnClasspath("js", "react-dom-server.browser.production.min.js"),
      Source.create("js", "window = {console: console, location: {href: 'https://shipreq.com'}, navigator: {userAgent: ''}}"),
      SourceUtil.requireFileOnClasspath("js", "sample-sjr-spa.js"))

  val Render = Source.create("js", "A.comp2()")

  def warmup(engine: Engine, contexts: Int, totalRenders: Int): Unit = {
    if (contexts > 0) {
      val renders = totalRenders / contexts
      for (_ <- 1 to contexts) {
        val ctx = newCtx(engine)
        for (_ <- 1 to renders) {
          ctx.eval(Render)
        }
        closeCtx(ctx)
      }
    }
  }

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

//[info] Benchmark            (warmupContexts)  (warmupRenders)  Mode  Cnt   Score   Error  Units
//[info] WarmupTimeBM.warmup                 1             1680  avgt   10  20.888 ± 1.278   s/op
//[info] WarmupTimeBM.warmup                 2             1680  avgt   10  30.447 ± 1.715   s/op
//[info] WarmupTimeBM.warmup                 3             1680  avgt   10  33.835 ± 1.854   s/op
//[info] WarmupTimeBM.warmup                 4             1680  avgt   10  34.101 ± 1.516   s/op
//[info] WarmupTimeBM.warmup                 5             1680  avgt   10  34.159 ± 2.025   s/op
//[info] WarmupTimeBM.warmup                 6             1680  avgt   10  35.152 ± 2.013   s/op
//[info] WarmupTimeBM.warmup                 7             1680  avgt   10  34.315 ± 1.125   s/op
//[info] WarmupTimeBM.warmup                 8             1680  avgt   10  35.329 ± 3.176   s/op
//[info] WarmupTimeBM.warmup                10             1680  avgt   10  34.252 ± 2.553   s/op

//[info] Benchmark            (warmupContexts)  (warmupRenders)  Mode  Cnt   Score    Error  Units
//[info] WarmupTimeBM.warmup                 1             2000  avgt    4  21.488 ±  2.551   s/op
//[info] WarmupTimeBM.warmup                 2             2000  avgt    4  33.100 ±  3.985   s/op
//[info] WarmupTimeBM.warmup                10             2000  avgt    4  37.501 ± 10.205   s/op
//[info] WarmupTimeBM.warmup                20             2000  avgt    4  34.627 ±  7.568   s/op
//[info] WarmupTimeBM.warmup               100             2000  avgt    4  34.508 ±  9.125   s/op
//[info] WarmupTimeBM.warmup               200             2000  avgt    4  35.849 ±  6.542   s/op
//[info] WarmupTimeBM.warmup              1000             2000  avgt    4  52.369 ±  4.029   s/op
//[info] WarmupTimeBM.warmup              2000             2000  avgt    4  71.116 ± 12.226   s/op

@Warmup(iterations = 0)
@Measurement(iterations = 1, time = 1, timeUnit = TimeUnit.NANOSECONDS)
@Fork(10)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
class WarmupTimeBM {

  @Param(Array("1", "2", "3", "4", "5", "6", "7", "8", "10"))
  var warmupContexts: Int = _

  @Param(Array("1680"))
  var warmupRenders: Int = _

  @Setup def setup: Unit = {
    WarmupBM
    ()
  }

  @Benchmark def warmup = {
    val engine = Engine.newBuilder().build()
    WarmupBM.warmup(engine, warmupContexts, warmupRenders)
    engine
  }
}


//[info] Benchmark        (warmupContexts)  (warmupRenders)  Mode  Cnt    Score     Error  Units
//[info] WarmupBM.render                 0             1000  avgt   40  186.561 ± 146.699  ms/op
//[info] WarmupBM.render                 1             1000  avgt   40   73.349 ±  47.918  ms/op
//[info] WarmupBM.render                10             1000  avgt   40   18.536 ±   5.427  ms/op
//[info] WarmupBM.render               100             1000  avgt   40   11.542 ±   2.710  ms/op
//[info] WarmupBM.render              1000             1000  avgt   40    9.835 ±   4.310  ms/op

//[info] Benchmark        (warmupContexts)  (warmupRenders)  Mode  Cnt     Score    Error  Units
//[info] WarmupBM.render                 1             1680  avgt   40    70.871 ± 52.280  ms/op
//[info] WarmupBM.render                 2             1680  avgt   40    31.106 ± 15.262  ms/op
//[info] WarmupBM.render                 3             1680  avgt   40    27.365 ± 11.238  ms/op
//[info] WarmupBM.render                 4             1680  avgt   40    22.310 ±  8.398  ms/op
//[info] WarmupBM.render                 5             1680  avgt   40    19.572 ±  7.493  ms/op
//[info] WarmupBM.render                 6             1680  avgt   40    16.894 ±  5.099  ms/op
//[info] WarmupBM.render                 7             1680  avgt   40    17.199 ±  5.202  ms/op
//[info] WarmupBM.render                 8             1680  avgt   40    16.548 ±  4.275  ms/op

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
    val engine = Engine.newBuilder().build()
    warmup(engine, warmupContexts, warmupRenders)
    this.ctx = newCtx(engine)
  }

  @Benchmark def render = ctx.eval(Render)
}
