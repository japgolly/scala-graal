package japgolly.scalagraal

import utest._

object Demo extends TestSuite {

  override def tests = Tests {

    'demo {
      import GraalJs._
      val ctx = ContextSync()

      // 1. Pre-compile expression functions for fast invocation.
      // 2. Typeclasses determine how to translate and/or marshall data from Scala to JS.
      val expr: (Int, Int) => Expr[String] =
        Expr.compile2((a, b) => s"($a + $b) * 2 + '!'")(_.asString)

      val result = ctx.eval(expr(3, 8))
      assert(result == Right("22!"))
    }

  }
}
