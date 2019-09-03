package japgolly.scalagraal

import java.util.concurrent.{Future => JavaFuture, _}
import scala.concurrent.{ExecutionContext, Future}

object Effect {

  type Id[A] = A

  // ===================================================================================================================

  trait AsyncES[F[_]] {
    def apply(es: ExecutorService): Async[F]
  }

  object AsyncES {

    implicit def javaFutureInstance: AsyncES[JavaFuture] =
      Async.javaFutureInstance

    implicit def futureInstance: AsyncES[Future] =
      es => Async.futureInstance(ExecutionContext.fromExecutorService(es))

    def syncTimed(timeout: Long, timeUnit: TimeUnit): AsyncES[Option] =
      es => {
        val asyncJ = Async.javaFutureInstance(es)
        new Async[Option] {
          override def delay[A](a: => A) = {
            val f = asyncJ.delay(a)
            try
              Some(f.get(timeout, timeUnit))
            catch {
              case _: TimeoutException =>
                f.cancel(true)
                None
              case _: InterruptedException =>
                f.cancel(true)
                Thread.currentThread().interrupt()
                None
              case _: CancellationException =>
                None
            }
          }
        }
      }
  }

  // ===================================================================================================================

  trait Async[F[_]] {
    def delay[A](a: => A): F[A]
  }

  object Async {

    implicit def javaFutureInstance(es: ExecutorService): Async[JavaFuture] =
      new Async[JavaFuture] {
        override def delay[A](a: => A) = es.submit(new Callable[A] {override def call() = a})
      }

    implicit def futureInstance(implicit ec: ExecutionContext): Async[Future] =
      new Async[Future] {
        override def delay[A](a: => A) = Future(a)
      }
  }
}
