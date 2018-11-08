package japgolly.scalagraal

import utest._
import TestUtil._
import Expr.Param

object ExprTest extends TestSuite {

  val paramTypes = Vector[Int => Param[Int]](
    i => Param.Const(i.toString),
    _ => Param.Literal(_.toString),
    _ => Param.Polyglot(identity),
    _ => Param.Custom(i => _.eval(lang.name, i.toString)))

  override def tests = Tests {

    'args {

      "1" - {
        val a = 666

        for {
          pa <- paramTypes.map(_(a))
        } {
          val fn = Expr.compile1(identity)(lang, pa).andThen(_.asInt)
          val expr = fn(a)
          val result = sync(expr)
          assertEvalResult(result, a)
        }
      }

      "4" - {
        val mkExpr: (String, String, String, String) => String =
          (a, b, c, d) => s"1000*$a + 100*$b + 10*$c + $d"

        val (a, b, c, d) = (5, 4, 3, 2)
        val expect = 5432

        for {
          pa <- paramTypes.map(_(a))
          pb <- paramTypes.map(_(b))
          pc <- paramTypes.map(_(c))
          pd <- paramTypes.map(_(d))
        } {
          val fn = Expr.compile4(mkExpr, _.asInt)(lang, pa, pb, pc, pd)
          val expr = fn(a, b, c, d)
          val result = sync(expr)
          assertEvalResult(result, expect)
        }
      }

    }

  }
}
