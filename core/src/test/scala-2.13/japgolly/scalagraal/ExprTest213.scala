package japgolly.scalagraal

import org.graalvm.polyglot.Value
import utest._
import TestUtil._

object ExprTest213 extends TestSuite {

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
  }
}
