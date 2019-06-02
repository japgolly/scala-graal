package japgolly.scalagraal

import java.util.concurrent.{Future => JavaFuture, _}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.JavaConversions

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
      es => Async.futureInstance(JavaConversions.asExecutionContext(es))
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
