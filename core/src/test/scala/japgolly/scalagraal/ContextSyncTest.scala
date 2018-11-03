package japgolly.scalagraal

import japgolly.microlibs.testutil.TestUtil._
import org.graalvm.polyglot.Context
import scalaz.std.anyVal._
import scalaz.std.option._
import utest._

object ContextSyncTest extends TestSuite {

  override def tests = Tests {

    'eval {
      val sync = ContextSync.single(Context.create("js"))
      assertEq(sync(Expr("js", "(1+1) * 100").asInt).toOption, Some(200))
    }

  }
}
