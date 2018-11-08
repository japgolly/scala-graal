package japgolly.scalagraal

import TestUtil._
import utest._

object ContextSyncTest extends TestSuite {

  override def tests = Tests {

    'eval {
      assertEvalResult(sync(Expr("(1+1) * 100").asInt), 200)
    }

    'interpolation {
      'static {
        assertEvalResult(sync(js"(1+2+1) * 101".asInt), 404)
      }
      'twoInts {
        val a = 3
        val b = 8
        val expr = js"1+($a+$b)*2".asInt
        assertEvalResult(sync(expr), 23)
      }
      'option {
        val a = None
        val b = Some(456)
        val expr = js"($a == null) ? $b : $a".asOption(_.asInt)
        assertEvalResult(sync(expr), b)
      }
    }

    'demo {
      implicit val lang = Language.JS
      val ctx = ContextSync()

      val (a,b) = (3,8)
      val expr = js"($a + $b) * 2".asInt

      val Right(result) = ctx(expr)
      assert(result == 22)
    }

  }
}
