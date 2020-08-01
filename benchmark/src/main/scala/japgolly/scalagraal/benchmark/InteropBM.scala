package japgolly.scalagraal.benchmark

import boopickle.Default._
import japgolly.scalagraal.GraalBoopickle._
import japgolly.scalagraal._
import japgolly.scalagraal.benchmark.InteropBM._
import japgolly.scalagraal.js.GraalJs._
import japgolly.scalagraal.util.{Warmup => GraalWarmup}
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

object InteropBM {
  val SetupExpr: Expr[Unit] =
    Expr.runAll(
      Expr.requireFileOnClasspath("react.production.min.js"),
      Expr.requireFileOnClasspath("react-dom-server.browser.production.min.js"),
      Expr.requireFileOnClasspath("ext-boopickle-test-opt.js"),
      Expr("window = {console: console, location: {href: 'https://shipreq.com'}, navigator: {userAgent: ''}}"),
    )

  def render(a: String): String =
    s"ReactDOMServer.renderToStaticMarkup(React.createElement('div', null, $a))"

  final case class BinStr2(a: String, b: String)
  implicit val picklerBinStr2: Pickler[BinStr2] = generatePickler
}

@Warmup(iterations = 0)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.NANOSECONDS)
@Fork(5)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class InteropBM {

  @Param(Array("String", "Bin:String"))
  var argType: String = _

  @Param(Array("2"))
  var exprInputs: Int = _

  @Param(Array("Default")) //, "SourceFn", "ValueFn", "CtxValueFn"))
  var exprParam: String = _

  @Param(Array("10", "1000", "10000"))
  var warmupContexts: Int = _

  var run: () => Any = _

  @Setup def setup: Unit = {
    val ctx = GraalContext()

    ctx.eval(SetupExpr).left.foreach(e => throw e.underlying)

    run = argType match {

      // ===============================================================================================================
      case "Int" =>

        val ep: ExprParam[Int] = exprParam match {
            case "Default"    => implicitly
            case "SourceFn"   => ExprParam.SourceFn(_.toString)
            case "ValueFn"    => ExprParam.RawValueFn
            case "CtxValueFn" => ExprParam.CtxValueFn(i => _ => ExprParam.RawValue(i))
          }

        val expr: Int => Expr[String] = exprInputs match {
          case 1 => val e = Expr.apply1(a => render(s"($a + $a) * 2 + '!'")).compile(_.asString)(implicitly, ep); i => e(i)
          case 2 => val e = Expr.apply2((a, b) => render(s"($a + $b) * 2 + '!'")).compile(_.asString)(implicitly, ep, ep); i => e(i, i)
        }

        GraalWarmup.sync(ctx)(warmupContexts, expr(9))

        () => ctx.eval(expr(7))

      // ===============================================================================================================
      case "String" =>
        val ep: ExprParam[String] = exprParam match {
            case "Default"    => implicitly
            case "SourceFn"   => ExprParam.SourceFn("'" + _ + "'")
            case "ValueFn"    => ExprParam.RawValueFn
            case "CtxValueFn" => ExprParam.CtxValueFn(i => _ => ExprParam.RawValue(i))
          }

        val expr: String => Expr[String] = exprInputs match {
          case 1 => val e = Expr.apply1(a => render(s"$a + '!' + $a")).compile(_.asString)(implicitly, ep); i => e(i)
          case 2 => val e = Expr.apply2((a, b) => render(s"$a + '!' + $b")).compile(_.asString)(implicitly, ep, ep); i => e(i, i)
        }

        GraalWarmup.sync(ctx)(warmupContexts, expr("asd"))

        () => ctx.eval(expr("qweazxc"))

      // ===============================================================================================================
      case "Bin:String" =>
        val ep: ExprParam[BinStr2] = exprParam match {
          case "Default" => implicitly
        }

        val expr: BinStr2 => Expr[String] = exprInputs match {
          case 2 => val e = Expr.apply1(i => render(s"'' + $i")).compile(_.asString)(implicitly, ep); i => e(i)
        }

        GraalWarmup.sync(ctx)(warmupContexts, expr(BinStr2("a1", "a2")))

        () => ctx.eval(expr(BinStr2("x1", "x2")))
    }
  }

  @Benchmark def eval = run()
}
