package japgolly.scalagraal.benchmark

import japgolly.scalagraal.{Warmup => GraalWarmup, _}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import GraalJs._
import InteropBM._

object InteropBM {
  val SetupExpr: Expr[Unit] =
    Expr.stdlibCosequenceDiscard(List(
      Expr.requireFileOnClasspath("react.production.min.js"),
      Expr.requireFileOnClasspath("react-dom-server.browser.production.min.js"),
      Expr("window = {console: console, location: {href: 'https://shipreq.com'}, navigator: {userAgent: ''}}"),
    ))

  def render(a: String): String =
    s"ReactDOMServer.renderToStaticMarkup(React.createElement('div', null, $a))"
}

@Warmup(iterations = 0)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.NANOSECONDS)
@Fork(5)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class InteropBM {

  @Param(Array("Int", "String"))
  var argType: String = _

  @Param(Array("1", "2"))
  var exprInputs: Int = _

  @Param(Array("SourceFn", "ValueFn", "CtxValueFn"))
  var exprParam: String = _

  @Param(Array("10", "1000", "10000"))
  var warmupContexts: Int = _

  var run: () => Any = _

  @Setup def setup: Unit = {
    val ctx = ContextSync()

    ctx.eval(SetupExpr).left.foreach(e => throw e.underlying)

    run = argType match {
      case "Int" =>

        val ep: ExprParam[Int] = exprParam match {
            case "SourceFn"   => ExprParam.SourceFn(_.toString)
            case "ValueFn"    => ExprParam.RawValueFn
            case "CtxValueFn" => ExprParam.CtxValueFn(i => _ => ExprParam.RawValue(i))
          }

        val expr: Int => Expr[String] = exprInputs match {
          case 1 => val e = Expr.compile1(a => render(s"($a + $a) * 2 + '!'"))(_.asString)(implicitly, ep); i => e(i)
          case 2 => val e = Expr.compile2((a, b) => render(s"($a + $b) * 2 + '!'"))(_.asString)(implicitly, ep, ep); i => e(i, i)
        }

        GraalWarmup.sync(ctx)(warmupContexts, expr(9))

        () => ctx.eval(expr(7))

      case "String" =>

        val ep: ExprParam[String] = exprParam match {
            case "SourceFn"   => ExprParam.SourceFn("'" + _ + "'")
            case "ValueFn"    => ExprParam.RawValueFn
            case "CtxValueFn" => ExprParam.CtxValueFn(i => _ => ExprParam.RawValue(i))
          }

        val expr: String => Expr[String] = exprInputs match {
          case 1 => val e = Expr.compile1(a => render(s"$a + '!' + $a"))(_.asString)(implicitly, ep); i => e(i)
          case 2 => val e = Expr.compile2((a, b) => render(s"$a + '!' + $b"))(_.asString)(implicitly, ep, ep); i => e(i, i)
        }

        GraalWarmup.sync(ctx)(warmupContexts, expr("asd"))

        () => ctx.eval(expr("qweazxc"))
    }
  }

  @Benchmark def eval = run()
}