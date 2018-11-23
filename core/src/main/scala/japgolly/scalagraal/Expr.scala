package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

final class Expr[+A] private[Expr] (private[scalagraal] val run: Context => A) extends AbstractFunction1[Context, Expr.Result[A]] {

  override def apply(context: Context): Expr.Result[A] =
    try {
      Right(run(context))
    } catch {
      case t: ExprError => Left(t)
      case t: Throwable => throw t
    }

  def evalOrThrow(context: Context): A =
    run(context)

  def map[B](f: A => B): Expr[B] =
    new Expr(f compose run)

  def flatMap[B](f: A => Expr[B]): Expr[B] =
    new Expr(c => f(run(c)).run(c))

  @inline private def _as[B](f: Value => B)(implicit ev: Expr[A] <:< Expr[Value]): Expr[B] =
    ev(this).map(v => ExprError.InResult.capture(v, f))

  def asBoolean(implicit ev: Expr[A] <:< Expr[Value]): Expr[Boolean] = _as(_.asBoolean())
  def asByte   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Byte   ] = _as(_.asByte())
  def asDouble (implicit ev: Expr[A] <:< Expr[Value]): Expr[Double ] = _as(_.asDouble())
  def asFloat  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Float  ] = _as(_.asFloat())
  def asInt    (implicit ev: Expr[A] <:< Expr[Value]): Expr[Int    ] = _as(_.asInt())
  def asLong   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Long   ] = _as(_.asLong())
  def asShort  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Short  ] = _as(_.asShort())
  def asString (implicit ev: Expr[A] <:< Expr[Value]): Expr[String ] = _as(_.asString())

  def as[T](t: TypeLiteral[T])(implicit ev: Expr[A] <:< Expr[Value]): Expr[T] =
    _as(_.as(t))

  def as[T](implicit ev: Expr[A] <:< Expr[Value], ct: ClassTag[T]): Expr[T] = {
    val t = ct.runtimeClass.asInstanceOf[Class[T]]
    _as(_.as(t))
  }

  def void: Expr[Unit] =
    new Expr(c => {run(c); ()})

  def asOption[F, B](f: Expr[Value] => Expr[B])(implicit ev: Expr[A] <:< Expr[Value]): Expr[Option[B]] = {
    val self = ev(this)
    new Expr(c => {
      val v = self.run(c)
      if (ExprError.InResult.capture(v, _.isNull))
        None
      else
        Some(f(Expr.const(v)).run(c))
    })
  }

  def >>[B](next: Expr[B]): Expr[B] =
    new Expr(c => {run(c); next.run(c)})

  @inline def <<[B](prev: Expr[B]): Expr[A] =
    prev >> this

  def timed: Expr[(Duration, A)] =
    new Expr(ctx => {
      val start = System.nanoTime()
      val a = run(ctx)
      val end = System.nanoTime()
      val dur = Duration.ofNanos(end - start)
      (dur, a)
    })
}

object Expr extends ExprBoilerplate {
  type Result[+A] = Either[ExprError, A]

  def apply(source: CharSequence)(implicit language: Language): Expr[Value] =
    apply(Source.create(language.name, source))

  def apply(source: Source): Expr[Value] =
    new Expr(c => ExprError.InEval.capture(c.eval(source)))

  def lift[A](f: Context => A): Expr[A] =
    new Expr(f)

  def const[A](a: A): Expr[A] =
    new Expr(_ => a)

  def point[A](a: => A): Expr[A] =
    new Expr(_ => a)

  val unit: Expr[Unit] =
    new Expr(_ => ())

  def fail[A](e: ExprError): Expr[A] =
    new Expr(_ => throw e)

  def byName[A](e: => Expr[A]): Expr[A] =
    unit.flatMap(_ => e)

  def tailrec[A, B](init: => A)(f: A => Expr[Either[A, B]]): Expr[B] =
    lift[B] { ctx =>
      @tailrec def go(a1: A): B =
        f(a1).run(ctx) match {
          case Left(a2) => go(a2)
          case Right(b) => b
        }
      go(init)
    }

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

  def fileOnClasspath(filename: String)(implicit lang: Language): Option[Expr[Value]] =
    SourceUtil.fileOnClasspath(filename).map(apply)

  def fileOnClasspath(lang: String, filename: String): Option[Expr[Value]] =
    SourceUtil.fileOnClasspath(lang, filename).map(apply)

  def requireFileOnClasspath(filename: String)(implicit lang: Language): Expr[Value] =
    apply(SourceUtil.requireFileOnClasspath(filename))

  def requireFileOnClasspath(lang: String, filename: String): Expr[Value] =
    apply(SourceUtil.requireFileOnClasspath(lang, filename))

  override protected def genericOpt[Z](params: Array[ExprParam[X]],
                                       mkExprStr: Array[String] => String,
                                       post: Expr[Value] => Z)
                                      (implicit l: Language): Array[X] => Z = {
    val arity = params.length

    def mkRun(args: Array[X], usesBindings: Boolean): Context => Value = {
      val tokens = new Array[String](arity)
      var i = arity
      while (i > 0) {
        i -= 1
        val token: String = params(i) match {
          case p: ExprParam.SourceConst[X] => p.source
          case _: ExprParam.ValueFn    [X] => l.argElement(i)
          case _: ExprParam.CtxValueFn [X] => l.argElement(i)
          case p: ExprParam.SourceFn   [X] => p.mkSource(args(i))
        }
        tokens(i) = token
      }
      val es = mkExprStr(tokens)
      val src = Source.create(l.name, es)
      if (usesBindings) {
        val run = l.argBinder(src)
        c => ExprError.InEval.capture(run(c))
      } else
        c => ExprError.InEval.capture(c.eval(src))
    }

    def mkValuesCtxFree(args: Array[X]): Array[Any] = {
      val values = new Array[Any](arity)
      var i = arity
      while (i > 0) {
        i -= 1
        params(i) match {
          case p: ExprParam.ValueFn    [X] => values(i) = p.mkValue(args(i))
          case _: ExprParam.CtxValueFn [X]
             | _: ExprParam.SourceFn   [X]
             | _: ExprParam.SourceConst[X] => ()
        }
      }
      values
    }

    def mkValuesWithCtx(args: Array[X]): List[(Array[Any], Context) => Unit] = {
      var fs = List.empty[(Array[Any], Context) => Unit]
      var j = arity
      while (j > 0) {
        j -= 1
        val i = j
        params(i) match {
          case p: ExprParam.ValueFn    [X] => val v = p.mkValue(args(i)); fs ::= ((tgt, _) => tgt(i) = v)
          case p: ExprParam.CtxValueFn [X] => val g = p.mkValue(args(i)); fs ::= ((tgt, c) => tgt(i) = g(c))
          case _: ExprParam.SourceConst[X]
             | _: ExprParam.SourceFn   [X] => ()
        }
      }
      fs
    }

    def mkExprWithBindings(run: Context => Value, args: Array[X], hasCtxValueFn: Boolean): Z =
      post(
        if (hasCtxValueFn) {
          val setValueFns = mkValuesWithCtx(args)
          lift { ctx =>
            val data = new Array[Any](arity)
            setValueFns.foreach(_ (data, ctx))
            l.argBinding.withValue(ctx, data)(run(ctx))
          }
        } else {
          val data = mkValuesCtxFree(args)
          lift { ctx =>
            l.argBinding.withValue(ctx, data)(run(ctx))
          }
        }
      )

    var hasSourceFn, hasValueFn, hasCtxValueFn = false
    params.foreach {
      case _: ExprParam.SourceConst[X] => ()
      case _: ExprParam.SourceFn   [X] => hasSourceFn = true
      case _: ExprParam.ValueFn    [X] => hasValueFn = true
      case _: ExprParam.CtxValueFn [X] => hasCtxValueFn = true
    }
    val usesBindings = hasValueFn || hasCtxValueFn

    if (usesBindings) {
      if (hasSourceFn) {
        args => {
          val run = mkRun(args, usesBindings = usesBindings)
          mkExprWithBindings(run, args, hasCtxValueFn = hasCtxValueFn)
        }
      } else {
        val run = mkRun(null, usesBindings = usesBindings)
        args => mkExprWithBindings(run, args, hasCtxValueFn = hasCtxValueFn)
      }
    } else {
      if (hasSourceFn) {
        args => post(lift(mkRun(args, usesBindings = usesBindings)))
      } else {
        val expr = post(lift(mkRun(null, usesBindings = usesBindings)))
        _ => expr
      }
    }
  }
}
