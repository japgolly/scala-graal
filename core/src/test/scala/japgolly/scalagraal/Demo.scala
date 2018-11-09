package japgolly.scalagraal

import utest._

object Demo extends TestSuite {

  override def tests = Tests {

    'demo {
      import GraalJs._
      val ctx = ContextSync()

      val expr = Expr.compile2[Int, Int]((a, b) => s"($a + $b) * 2 + '!'")(_.asString)

      val result = ctx.eval(expr(3, 8))
      assert(result == Right("22!"))
    }

  }
}
