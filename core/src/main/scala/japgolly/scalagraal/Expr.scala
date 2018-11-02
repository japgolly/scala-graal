package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot.{Context, Source, TypeLiteral, Value}
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1
import scala.util.control.NonFatal

final class Expr[A](private[Expr] val run: Context => A) extends AbstractFunction1[Context, Expr.Result[A]] {
  override def apply(context: Context): Expr.Result[A] =
    try
      Right(run(context))
    catch {
      case NonFatal(t) => Left(t)
    }

  def map[B](f: A => B): Expr[B] =
    new Expr(f compose run)

  def flatMap[B](f: A => Expr[B]): Expr[B] =
    new Expr(c => f(run(c)).run(c))

  // @throws ClassCastException if this value could not be converted to string.
  // @throws UnsupportedOperationException if this value does not represent a string.
  // @throws PolyglotException if a guest language error occurred during execution.
  def asBoolean(implicit ev: Expr[A] =:= Expr[Value]): Expr[Boolean] = ev(this).map(_.asBoolean())
  def asByte   (implicit ev: Expr[A] =:= Expr[Value]): Expr[Byte   ] = ev(this).map(_.asByte())
  def asDouble (implicit ev: Expr[A] =:= Expr[Value]): Expr[Double ] = ev(this).map(_.asDouble())
  def asFloat  (implicit ev: Expr[A] =:= Expr[Value]): Expr[Float  ] = ev(this).map(_.asFloat())
  def asInt    (implicit ev: Expr[A] =:= Expr[Value]): Expr[Int    ] = ev(this).map(_.asInt())
  def asLong   (implicit ev: Expr[A] =:= Expr[Value]): Expr[Long   ] = ev(this).map(_.asLong())
  def asShort  (implicit ev: Expr[A] =:= Expr[Value]): Expr[Short  ] = ev(this).map(_.asShort())
  def asString (implicit ev: Expr[A] =:= Expr[Value]): Expr[String ] = ev(this).map(_.asString())

  def as[T](t: TypeLiteral[T])(implicit ev: Expr[A] =:= Expr[Value]): Expr[T] =
    ev(this).map(_.as(t))

  def as[T](implicit ev: Expr[A] =:= Expr[Value], ct: ClassTag[T]): Expr[T] = {
    val t = ct.runtimeClass.asInstanceOf[Class[T]]
    ev(this).map(_.as(t))
  }

  def asOption[B](f: Expr[Value] => Expr[B])(implicit ev: Expr[A] =:= Expr[Value]): Expr[Option[B]] = {
    val self = ev(this)
    new Expr(c => {
      val v = self.run(c)
      if (v.isNull) None else Some(f(Expr.const(v)).run(c))
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
  type Error = Throwable
  type Result[+A] = Either[Error, A]

  def apply(js: CharSequence): Expr[Value] =
    apply(Source.create("js", js))

  def apply(source: Source): Expr[Value] = {
    // @throws PolyglotException in case parsing or evaluation of the guest language code failed.
    // @throws IllegalStateException if the context is already closed, the current thread is not
    //             allowed to access this context
    // @throws IllegalArgumentException if the language of the given source is not installed or the
    //             {@link Source#getMimeType() MIME type} is not supported with the language.
    new Expr(_.eval(source))
  }

  def const[A](a: A): Expr[A] =
    new Expr(_ => a)

  def point[A](a: => A): Expr[A] =
    new Expr(_ => a)
}
