package japgolly.scalagraal

import TestUtil.{sync => _, _}
import org.graalvm.polyglot.Context
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scalaz.Equal
import utest._

object ContextPoolTest extends TestSuite {

  implicit val stateEq: Equal[ContextPool.State] = Equal.equalA

  override def tests = Tests {

    'poolOf2 {
      val mutex = new AnyRef
      var threadNames = Set.empty[String]

      val pool = ContextPool.Builder
          .fixedThreadPool(2)
          .fixedContextPerThread()
          .configure(_.afterContextCreate(Expr.point{mutex.synchronized(threadNames += Thread.currentThread().getName)}))
          .build()

      assertEq(pool.poolState(), ContextPool.State.Active)

      for (_ <- 1 to 4) {
        val fa = pool.eval(Expr("(1+1) * 100").asInt)
        val fb = pool.eval(Expr("10 * (5+3)").asInt)

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

      assert(pool.poolState() == ContextPool.State.Active)
      pool.shutdown()
      eventually(pool.poolState() == ContextPool.State.Terminated)
    }

  }
}
