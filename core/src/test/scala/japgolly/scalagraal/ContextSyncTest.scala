package japgolly.scalagraal

import japgolly.microlibs.testutil.TestUtil._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.either._
import scalaz.std.option._
import scalaz.std.string._
import utest._

object ContextSyncTest extends TestSuite {

  implicit val lang = Language.JS

  def assertEvalResult[A: Equal](actual: Expr.Result[A], expect: A): Unit =
    assertEq(actual.left.map(_.toString), Right(expect))

  override def tests = Tests {

    val sync = ContextSync()

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
