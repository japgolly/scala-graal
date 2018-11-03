package japgolly.scalagraal

import japgolly.microlibs.testutil.TestUtil._
import org.graalvm.polyglot.Context
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.set._
import scalaz.std.string._
import scalaz.std.tuple._
import utest._

object ContextAsyncTest extends TestSuite {

  override def tests = Tests {

    'poolOf2 {
      val mutex = new AnyRef
      var threadNames = Set.empty[String]

      val pool = ContextAsync.fixedPool(2) {
        mutex.synchronized {
          threadNames += Thread.currentThread().getName
          Context.create("js")
        }
      }

      val fa = pool(Expr("js", "(1+1) * 100").asInt)
      val fb = pool(Expr("js", "10 * (5+3)").asInt)

      implicit val ec = ExecutionContext.global

      val f = for {
        ra <- fa
        rb <- fb
      } yield for {
        a <- ra
        b <- rb
      } yield (a, b)

      val a = Await.result(f, 1 second)
      assertEq(a.toOption, Some((200, 80)))

      assertEq(threadNames, Set("ScalaGraal-pool-1-thread-1", "ScalaGraal-pool-1-thread-2"))
    }

  }
}
