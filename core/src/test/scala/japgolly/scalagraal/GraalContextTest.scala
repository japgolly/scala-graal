package japgolly.scalagraal

import japgolly.scalagraal.TestUtil._
import utest._

object GraalContextTest extends TestSuite {

  override def tests = Tests {

    "eval" - {
      assertEvalResult(sync.eval(Expr("(1+1) * 100").asInt), 200)
    }

    "evalWithStats" - {
      val r = sync.evalWithStats(Expr("(1+1) * 100").asInt)
      assertEvalResult(r.result, 200)
      assert(r.metrics.total.nanos != 0)
      r.metrics.total.toStrNs
    }

  }
}
