package japgolly.scalagraal

import java.util.concurrent.atomic.AtomicInteger
import utest._
import TestUtil._

object WarmupTest extends TestSuite {

  override def tests = Tests {

    "single" - {
      val counter = new AtomicInteger(0)
      val expr = Expr.point(counter.incrementAndGet())
      val result = Warmup.sync(sync)(3, expr, _.outerReps == 2)
      assertEq("outerReps", result.outerReps, 2)
      assertEq("totalInnerReps", result.totalInnerReps, 6)
      assertEq("counter", counter.get(), 6)
      result
    }

    "pool" - {
      val counter = new AtomicInteger(0)
      val expr = Expr.point(counter.incrementAndGet())
      val pool = ContextPool.Builder.fixedThreadPool(2).fixedContextPerThread().build()
      val result = Warmup.pool(pool)(3, expr, _.outerReps == 3)

      import scala.concurrent.ExecutionContext.Implicits.global
      for {
        warm <- result.warm
        done <- result.done
      } yield {
        pool.unsafeShutdown()
        assertEq("warm.outerReps", warm.outerReps, 3)
        if (done.outerReps != 4 && done.outerReps != 3)
          fail("done.outerReps should be 3 or 4")
        done
      }
    }

  }
}
