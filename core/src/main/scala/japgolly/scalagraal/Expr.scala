package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

final class Expr[A](private[Expr] val run: Context => A) extends AbstractFunction1[Context, Expr.Result[A]] {

  override def apply(context: Context): Expr.Result[A] =
    try
      Right(run(context))
    catch {
      case t: ExprError => Left(t)
      case t: Throwable => throw t
    }

  def map[B](f: A => B): Expr[B] =
    new Expr(f compose run)

  def flatMap[B](f: A => Expr[B]): Expr[B] =
    new Expr(c => f(run(c)).run(c))

  @inline private def _as[B](f: Value => B)(implicit ev: Expr[A] =:= Expr[Value]): Expr[B] =
    ev(this).map(v => ExprError.InResult.capture(v, f))

  def asBoolean(implicit ev: Expr[A] =:= Expr[Value]): Expr[Boolean] = _as(_.asBoolean())
  def asByte   (implicit ev: Expr[A] =:= Expr[Value]): Expr[Byte   ] = _as(_.asByte())
  def asDouble (implicit ev: Expr[A] =:= Expr[Value]): Expr[Double ] = _as(_.asDouble())
  def asFloat  (implicit ev: Expr[A] =:= Expr[Value]): Expr[Float  ] = _as(_.asFloat())
  def asInt    (implicit ev: Expr[A] =:= Expr[Value]): Expr[Int    ] = _as(_.asInt())
  def asLong   (implicit ev: Expr[A] =:= Expr[Value]): Expr[Long   ] = _as(_.asLong())
  def asShort  (implicit ev: Expr[A] =:= Expr[Value]): Expr[Short  ] = _as(_.asShort())
  def asString (implicit ev: Expr[A] =:= Expr[Value]): Expr[String ] = _as(_.asString())

  def as[T](t: TypeLiteral[T])(implicit ev: Expr[A] =:= Expr[Value]): Expr[T] =
    _as(_.as(t))

  def as[T](implicit ev: Expr[A] =:= Expr[Value], ct: ClassTag[T]): Expr[T] = {
    val t = ct.runtimeClass.asInstanceOf[Class[T]]
    _as(_.as(t))
  }

  def asOption[F, B](f: Expr[Value] => Expr[B])(implicit ev: Expr[A] =:= Expr[Value]): Expr[Option[B]] = {
    val self = ev(this)
    new Expr(c => {
      val v = self.run(c)
      if (ExprError.InResult.capture(v, _.isNull))
        None
      else
        Some(f(Expr.const(v)).run(c))
    })
  }

  def timed: Expr[(Duration, A)] =
    new Expr(ctx => {
      val start = System.nanoTime()
      val a = run(ctx)
      val end = System.nanoTime()
      val dur = Duration.ofNanos(end - start)
      (dur, a)
    })
}

object Expr {
  type Result[A] = Either[ExprError, A]

  def apply(source: Source): Expr[Value] =
    new Expr(c => ExprError.InEval.capture(c.eval(source)))

  def const[A](a: A): Expr[A] =
    new Expr(_ => a)

  def point[A](a: => A): Expr[A] =
    new Expr(_ => a)
}

sealed trait ExprError extends RuntimeException {
  val underlying: Throwable
}

object ExprError {

  // ===================================================================================================================

  sealed trait InEval extends ExprError

  object InEval {
    def capture[A](a: => A): A =
      try a catch { case t: Throwable => throw unapply(t).getOrElse(t) }

    def unapply(t: Throwable): Option[InEval] = t match {
      case t: InEval                        => Some(t)
      case t: IllegalArgumentException      => Some(ExprError.UnsupportedLanguageOrMimeType(t))
      case t: IllegalStateException         => Some(ExprError.ContextClosed(t))
      case t: PolyglotException             => Some(ExprError.EvalError(t))
    }
  }

  final case class EvalError(underlying: PolyglotException) extends InEval

  final case class ContextClosed(underlying: IllegalStateException) extends InEval

  final case class UnsupportedLanguageOrMimeType(underlying: IllegalArgumentException) extends InEval

  // ===================================================================================================================

  sealed trait InResult extends ExprError {
    val value: Value
  }

  object InResult {
    def capture[A](v: Value, f: Value => A): A =
      try f(v) catch { case t: Throwable => throw unapply(t).fold(t)(_(v)) }

    def unapply(t: Throwable): Option[Value => InResult] = t match {
      case t: InResult                      => Some(_ => t)
      case t: PolyglotException             => Some(ExprError.ValueError(_, t))
      case t: NullPointerException          => Some(ExprError.ValueIsNull(_, t))
      case t: ClassCastException            => Some(ExprError.ValueCastError(_, t))
      case t: UnsupportedOperationException => Some(ExprError.ValueReprError(_, t))
    }
  }

  /** a guest language error occurred during execution */
  final case class ValueError(value: Value, underlying: PolyglotException) extends InResult

  final case class ValueIsNull(value: Value, underlying: NullPointerException) extends InResult

  /** the value could not be converted to the expected type */
  final case class ValueCastError(value: Value, underlying: ClassCastException) extends InResult

  /** the value does not represent the expected type */
  final case class ValueReprError(value: Value, underlying: UnsupportedOperationException) extends InResult
}
