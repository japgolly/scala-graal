package japgolly.scalagraal

import utest._

object Demo extends TestSuite {

  override def tests = Tests {

    "demo" - {
      // Use semantics and implicit config for JS
      // (Graal also supports Python, R, Ruby, LLVM)
      import GraalJs._

      // 1. Pre-compile expression functions for fast invocation.
      // 2. Typeclasses determine how to translate and/or marshall data from Scala to JS.
      val expr: (Int, Int) => Expr[String] =
        Expr.apply2((a, b) => s"($a + $b) * 2 + '!'").compile(_.asString)

      // Let's use a single synchronous JS evaluator/environment
      val ctx = ContextSync()

      val result = ctx.eval(expr(3, 8))
      assert(result == Right("22!"))
    }

  }
}
