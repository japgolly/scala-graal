package japgolly.scalagraal

import utest._
import TestUtil._

object ContextSyncTest extends TestSuite {

  override def tests = Tests {

    'eval {
      assertEvalResult(sync.eval(Expr("(1+1) * 100").asInt), 200)
    }

    'evalWithStats {
      val (r, s) = sync.evalWithStats(Expr("(1+1) * 100").asInt)
      assertEvalResult(r, 200)
      assert(s.total.nanos != 0)
      s.total.toStrNs
    }

  }
}
