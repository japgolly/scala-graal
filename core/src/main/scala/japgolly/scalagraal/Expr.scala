package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

object Example {
  val x = Expr(???).asString
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

//  // @throws ClassCastException if this value could not be converted to string.
//  // @throws UnsupportedOperationException if this value does not represent a string.
//  // @throws PolyglotException if a guest language error occurred during execution.
//  def asBoolean(implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Boolean] = ev(this).map(_.asBoolean())
//  def asByte   (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Byte   ] = ev(this).map(_.asByte())
//  def asDouble (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Double ] = ev(this).map(_.asDouble())
//  def asFloat  (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Float  ] = ev(this).map(_.asFloat())
//  def asInt    (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Int    ] = ev(this).map(_.asInt())
//  def asLong   (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Long   ] = ev(this).map(_.asLong())
//  def asShort  (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Short  ] = ev(this).map(_.asShort())
//  def asString (implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, String ] = ev(this).map(_.asString())

  def asString(implicit ev: Expr[E, A] =:= Expr[E, Value], m: ExprError.Merge[E, ExprError.InRead]): Expr[m.C, String] =
    ev(this).eflatmap {
      case Right(v) =>
        try
          Right(v.asString())
        catch {
          case e: ClassCastException            => Left(m.b(ExprError.ValueCastError(v, e)))
          case e: UnsupportedOperationException => Left(m.b(ExprError.ValueReprError(v, e)))
          case e: PolyglotException             => Left(m.b(ExprError.GuestLangError(v, e)))
        }
      case Left(e) =>
        Left(m.a(e))
    }

//  def as[T](t: TypeLiteral[T])(implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, T] =
//    ev(this).map(_.as(t))
//
//  def as[T](implicit ev: Expr[E, A] =:= Expr[E, Value], ct: ClassTag[T]): Expr[E, T] = {
//    val t = ct.runtimeClass.asInstanceOf[Class[T]]
//    ev(this).map(_.as(t))
//  }
//
//  def asOption[B](f: Expr[E, Value] => Expr[E, B])(implicit ev: Expr[E, A] =:= Expr[E, Value]): Expr[E, Option[B]] = {
//    val self = ev(this)
//    new Expr(c => {
//      val v = self.run(c)
//      if (v.isNull) None else Some(f(Expr.const(v)).run(c))
//    })
//  }
//
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
    type C
    val a: A => C
    val b: B => C
  }
  object Merge {
    type Aux[A, B, CC] = Merge[A, B] { type C = CC }
    def apply[A, B, CC](x: A => CC, y: B => CC): Aux[A, B, CC] = new Merge[A, B] { type C = CC; val a = x; val b = y}

    implicit def lub[A <: AnyRef]: Aux[A, A, A] = Merge(a => a, a => a)
  }

//  trait Merge[A, B] {
//    type C
//    val a: A => C
//    val b: B => C
//  }
//  trait MergePri2 {
//    implicit def lub[A, B <: A, C <: A]: Merge.Aux[B, C, A] = Merge(a => a, a => a)
//  }
//  trait MergePri1 extends MergePri2 {
//    implicit def lubL[A, B <: A]: Merge.Aux[A, B, A] = Merge(a => a, a => a)
//    implicit def lubR[A, B >: A]: Merge.Aux[A, B, B] = Merge(a => a, a => a)
//  }
//  object Merge extends MergePri1 {
//    type Aux[A, B, CC] = Merge[A, B] { type C = CC }
//    def apply[A, B, CC](x: A => CC, y: B => CC): Aux[A, B, CC] = new Merge[A, B] { type C = CC; val a = x; val b = y}
//
//    implicit def refl[A]: Aux[A, A, A] = Merge(a => a, a => a)
//  }
}
