package japgolly.scalagraal

import utest._
import TestUtil._

object ContextSyncTest extends TestSuite {

  override def tests = Tests {

    'eval {
      assertEvalResult(sync(Expr("(1+1) * 100").asInt), 200)
    }

  }
}
