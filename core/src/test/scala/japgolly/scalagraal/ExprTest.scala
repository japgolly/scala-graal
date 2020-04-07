package japgolly.scalagraal

import org.graalvm.polyglot.Value
import utest._
import TestUtil._

object ExprTest extends TestSuite {

  private val paramTypes = Vector[Int => ExprParam[Int]](
    i => ExprParam.SourceConst(i.toString),
    _ => ExprParam.SourceFn(_.toString),
    _ => ExprParam.ValueFn(ExprParam.RawValue),
    _ => ExprParam.CtxValueFn(i => ctx => ExprParam.RawValue(ctx.eval(graalLanguage.name, i.toString))))

  private trait X

  private class TestType[E] {
    def apply[A](a: A)(implicit ev: A =:= E) = ()
  }

  private def assertTypeIs[E] = new TestType[E]

  override def tests = Tests {

    "dsl" - {
      "ap1" - assertTypeIs[Expr[Value]               ](Expr.apply2(_ + "+" + _, 1, 2))
      "ap2" - assertTypeIs[(Int, Int) => Expr[Value] ](Expr.apply2[Int, Int](_ + "+" + _).compile)
      "ap3" - assertTypeIs[(Int, Int) => Expr[String]](Expr.apply2[Int, Int](_ + "+" + _).compile(_.asString))
      "fn1" - assertTypeIs[Expr[Value]               ](Expr.fn2("add", 1, 2))
      "fn2" - assertTypeIs[(Int, Int) => Expr[Value] ](Expr.fn2[Int, Int]("add").compile)
      "fn3" - assertTypeIs[(Int, Int) => Expr[String]](Expr.fn2[Int, Int]("add").compile(_.asString))
    }

    "args" - {

      "1" - {
        val a = 666

        for {
          pa <- paramTypes.map(_(a))
        } {
          val fn = Expr.apply1[Int](identity).compile(_.asInt)(graalLanguage, pa)
          val expr = fn(a)
          val result = sync.eval(expr)
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
          val expr = Expr.apply4(mkExpr)(a, b, c, d)(graalLanguage, pa, pb, pc, pd).asInt
          val result = sync.eval(expr)
          assertEvalResult(result, expect)
        }
      }

      "any" - {
        compileError(""" Expr.compile1(a => a)(_.asInt) """)
        ()
      }

      "X" - {
        compileError(""" Expr.compile1[X](a => a)(_.asInt) """)
        ()
      }
    }

    "assignToNewVar" - {
      val expr = Expr("123").asInt.assignToNewVar("x").void >> Expr("x").asInt
      assertEvalResult(sync.eval(expr), 123)
    }

    "results" - {
      "option" - {
        val a = Option.empty[Int]
        val b = Some(456)
        val expr = Expr.apply2((a, b) => s"($a == null) ? $b : $a", a, b).asOption(_.asInt)
        assertEvalResult(sync.eval(expr), b)
      }

      "stringList" - {
        val expr = Expr("['omg', 'hello']").asStringVector
        assertEvalResult(sync.eval(expr), Vector("omg", "hello"))
      }

      "asyncAwait" - {
        val expr = Expr("async function x(){return 123}; x()").asPromise(_.asInt()).await
        assertEvalResult(sync.eval(expr), 123)
      }
    }

    "errors" - {
      def test[A](e: Expr[A]) = {
        val r = sync.eval(e)
        assert(r.isLeft)
        // r.left.get.printStackTrace()
        r.left.getOrElse(???)
      }

      "eval"    - test(Expr("xxx"))
      "compile" - test(Expr.apply1(_ => "xxx", 1))
    }

  }
}
