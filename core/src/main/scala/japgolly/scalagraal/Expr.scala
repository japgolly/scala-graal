package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

final class Expr[A] private[Expr] (private[Expr] val run: Context => A) extends AbstractFunction1[Context, Expr.Result[A]] {

  override def apply(context: Context): Expr.Result[A] =
    try {
      context.enter()
      Right(run(context))
    } catch {
      case t: ExprError => Left(t)
      case t: Throwable => throw t
    } finally
      context.leave()

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

  def apply(lang: String, source: CharSequence): Expr[Value] =
    apply(Source.create(lang, source))

  def apply(source: Source): Expr[Value] =
    new Expr(c => ExprError.InEval.capture(c.eval(source)))

  def lift[A](f: Context => A): Expr[A] =
    new Expr(f)

  def const[A](a: A): Expr[A] =
    new Expr(_ => a)

  def point[A](a: => A): Expr[A] =
    new Expr(_ => a)

  def stdlibDist[F[x] <: Traversable[x], A, B](fa: F[A])(f: A => Expr[B])
                                              (implicit cbf: CanBuildFrom[F[A], B, F[B]]): Expr[F[B]] =
    lift(c => {
      val b = cbf(fa)
      fa.foreach(a => b += f(a).run(c))
      b.result()
    })

  def stdlibCosequence[F[x] <: Traversable[x], A](fea: F[Expr[A]])
                                                 (implicit cbf: CanBuildFrom[F[Expr[A]], A, F[A]]): Expr[F[A]] =
    stdlibDist[F, Expr[A], A](fea)(identity)
}
