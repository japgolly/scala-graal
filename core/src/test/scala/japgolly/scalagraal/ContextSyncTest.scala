package japgolly.scalagraal

import japgolly.microlibs.testutil.TestUtil._
import org.graalvm.polyglot.Context
import scalaz.std.anyVal._
import scalaz.std.option._
import utest._

object ContextSyncTest extends TestSuite {

  implicit val lang = Language.JS

  override def tests = Tests {

    val sync = ContextSync.single(Context.create("js"))

    'eval {
      assertEq(sync(Expr("(1+1) * 100").asInt).toOption, Some(200))
    }

    'interpolation {
      'static {
        assertEq(sync(js"(1+2+1) * 101".asInt).toOption, Some(404))
      }
      'twoInts {
        val a = 3
        val b = 8
        val expr = js"1+($a+$b)*2".asInt
        assertEq(sync(expr).toOption, Some(23))
      }
    }
  }
}
