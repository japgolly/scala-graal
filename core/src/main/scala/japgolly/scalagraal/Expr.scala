package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

object Example {
  val x = Expr(???).asString
  x: Expr[ExprError.InEvalOrRead, String]
  val y = x.flatMap(_ => Expr(???).asInt)
  y: Expr[ExprError.InEvalOrRead, Int]
}

final class Expr[E, A](private[Expr] val run: Context => Expr.Result[E, A]) extends AbstractFunction1[Context, Expr.Result[E, A]] {

  override def apply(context: Context): Expr.Result[E, A] =
    run(context)

  def map[B](f: A => B): Expr[E, B] =
    new Expr(run(_).map(f))

  def emap[B](f: A => Either[E, B]): Expr[E, B] =
    new Expr(run(_).flatMap(f))

  def eflatmap[F, B](f: Either[E, A] => Either[F, B]): Expr[F, B] =
    new Expr(f compose run)

  def flatMap[B](f: A => Expr[E, B]): Expr[E, B] =
    new Expr(c => run(c).flatMap(f(_).run(c)))
//  def flatMap[F, B](f: A => Expr[F, B])(implicit m: ExprError.Merge[E, F]): Expr[m.Out, B] =
//    new Expr(c => run(c) match {
//      case Right(a) => f(a).run(c) match {
//        case Right(b) => Right(b)
//        case Left(e) => Left(m.b(e))
//      }
//      case Left(e) => Left(m.a(e))
//    })

  def leftMap[F](f: E => F): Expr[F, A] =
    new Expr(run(_).left.map(f))

  def leftWiden[F >: E]: Expr[F, A] =
    this.asInstanceOf[Expr[F, A]]

  private def _as[B](f: Value => B)(implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, B] =
    ev(this).eflatmap {
      case Right(v) =>
        try
          Right(f(v))
        catch {
          case e: ClassCastException            => Left(m.b(ExprError.ValueCastError(v, e)))
          case e: NullPointerException          => Left(m.b(ExprError.ValueIsNull(v, e)))
          case e: UnsupportedOperationException => Left(m.b(ExprError.ValueReprError(v, e)))
          case e: PolyglotException             => Left(m.b(ExprError.GuestLangError(v, e)))
        }
      case Left(e) =>
        Left(m.a(e))
    }

  def asBoolean(implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Boolean] = _as(_.asBoolean())(ev, m)
  def asByte   (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Byte   ] = _as(_.asByte())(ev, m)
  def asDouble (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Double ] = _as(_.asDouble())(ev, m)
  def asFloat  (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Float  ] = _as(_.asFloat())(ev, m)
  def asInt    (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Int    ] = _as(_.asInt())(ev, m)
  def asLong   (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Long   ] = _as(_.asLong())(ev, m)
  def asShort  (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, Short  ] = _as(_.asShort())(ev, m)
  def asString (implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, String ] = _as(_.asString())(ev, m)

  def as[T](t: TypeLiteral[T])(implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.Out, T] =
    _as(_.as(t))(ev, m)

  def as[T](implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead], ct: ClassTag[T]): Expr[m.Out, T] = {
    val t = ct.runtimeClass.asInstanceOf[Class[T]]
    _as(_.as(t))(ev, m)
  }

  def asOption[F, B](f: Expr[E, Value] => Expr[F, B])
                    (implicit ev: Expr[E, A] =:= Expr[E, Value],
                     m1: ExprError.Merge[E, F],
                     m2: ExprError.Merge[m1.Out, ExprError.InRead]): Expr[m2.Out, Option[B]] = {
    val self = ev(this)
    new Expr(c =>
      self.run(c) match {
        case Right(v) =>
          if (v.isNull) Right(None) else f(Expr.const(v).leftWiden[E]).run(c).map(Some(_))
    })
  }

//  def timed: Expr[E, (Duration, A)] =
//    new Expr(ctx => {
//      val start = System.nanoTime()
//      val a = run(ctx)
//      val end = System.nanoTime()
//      val dur = Duration.ofNanos(end - start)
//      (dur, a)
//    })
}

object Expr {
  type Result[E, A] = Either[E, A]

  def apply(source: Source): Expr[ExprError.InEval, Value] =
    new Expr(c =>
      try
        Right(c.eval(source))
      catch {
        case e: PolyglotException => Left(ExprError.EvalError(e))
        case e: IllegalStateException => Left(ExprError.ContextClosed(e))
        case e: IllegalArgumentException => Left(ExprError.UnsupportedLanguageOrMimeType(e))
      }
    )

  def const[A](a: A): Expr[Nothing, A] = {
    val r = Right(a)
    new Expr(_ => r)
  }

  def point[A](a: => A): Expr[Nothing, A] =
    new Expr(_ => Right(a))
}

object ExprError {

  sealed trait InEvalOrRead

  sealed trait InEval extends InEvalOrRead

  final case class EvalError(underlying: PolyglotException) extends InEval
  final case class ContextClosed(underlying: IllegalStateException) extends InEval
  final case class UnsupportedLanguageOrMimeType(underlying: IllegalArgumentException) extends InEval

  sealed trait InRead extends InEvalOrRead {
    val value: Value
    val underlying: Throwable
  }

  final case class ValueIsNull(value: Value, underlying: NullPointerException) extends InRead

  /** the value could not be converted to the expected type */
  final case class ValueCastError(value: Value, underlying: ClassCastException) extends InRead

  /** the value does not represent the expected type */
  final case class ValueReprError(value: Value, underlying: UnsupportedOperationException) extends InRead

  /** a guest language error occurred during execution */
  final case class GuestLangError(value: Value, underlying: PolyglotException) extends InRead

  trait Merge[-A, -B] {
    type Out
    val a: A => Out
    val b: B => Out
  }
  trait MergeFallback {
    implicit def coproduct[A, B]: Merge.Aux[A, B, Either[A, B]] = Merge(Left(_), Right(_))
  }
  object Merge {
    type Aux[A, B, O] = Merge[A, B] { type Out = O }
    def apply[A, B, O](x: A => O, y: B => O): Aux[A, B, O] = new Merge[A, B] { type Out = O; val a = x; val b = y}

    implicit def lub[A <: AnyRef]: Aux[A, A, A] = Merge(a => a, a => a)
  }
}
