package japgolly.scalagraal

import japgolly.scalagraal.TestUtil.{sync => _, _}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scalaz.Equal
import utest._

object GraalContextPoolTest extends TestSuite {

  private implicit val stateEq: Equal[GraalContextPool.State] = Equal.equalA
  private implicit val resultEq: Equal[Option[Expr.Result[String]]] = Equal.equalA

  override def tests = Tests {

    "poolOf2" - {
      val mutex = new AnyRef
      var threadNames = Set.empty[String]

      val pool = GraalContextPool.Builder
          .fixedThreadPool(2)
          .fixedContextPerThread()
          .configure(_.onContextCreate(Expr.point{mutex.synchronized(threadNames += Thread.currentThread().getName)}))
          .build()

      assertEq(pool.unsafePoolState(), GraalContextPool.State.Active)

      val threadPoolRegex = "ScalaGraal-pool-(\\d+)-thread-.*".r
      var threadPool = Option.empty[Int]

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

        if (threadPool.isEmpty)
          threadNames.head match {
            case threadPoolRegex(p) => threadPool = Some(p.toInt)
          }

        val expectedThreadNames = Set(1, 2).map(i => s"ScalaGraal-pool-${threadPool.get}-thread-$i")
        assertSet(threadNames, expectedThreadNames)
      }

      assert(pool.unsafePoolState() == GraalContextPool.State.Active)
      pool.unsafeShutdown()
      eventually(pool.unsafePoolState() == GraalContextPool.State.Terminated)
    }

    "awaitResultsWithTimeout" - {
      val pool = GraalContextPool.Builder
        .fixedThreadPool(1)
        .fixedContextPerThread()
        .awaitResultsWithTimeout(100, TimeUnit.MILLISECONDS)
        .build()

      try {

        val fib =
          Expr.apply1[Int](n => s"function fibonacci(n){for(var r,c=1,f=0;0<=n;)r=c,c+=f,f=r,n--;return f}; fibonacci($n) + ''")
            .compile(_.asString)

        // scala> ctx.evalWithStats(fib(20000000))
        // res0 = ContextMetrics.AndResult(ContextMetrics(590 ms), Right(Infinity))
        val ko = pool.eval(fib(20000000))
        assertEq(ko, None)

        if (System.getenv("TRAVIS") == null) {
          val ok = pool.eval(fib(0))
          assertEq(ok, Some(Right("1")))
        }

      } finally {
        pool.unsafeShutdown()
      }
    }
  }
}
