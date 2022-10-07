package japgolly.scalagraal

import japgolly.scalagraal.util.DurationLite
import java.util.function.Consumer
import org.graalvm.polyglot.{Language => _, _}
import scala.annotation.tailrec
import scala.collection.BuildFrom
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1
import scala.util.Try

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

  def flatTap[B](f: A => Expr[B]): Expr[A] =
    for {
      a <- this
      _ <- f(a)
    } yield a

  def assignTo[AA >: A](name: String)(implicit e: ExprParam[AA], l: Language): Expr[AA] =
    flatTap(a => Expr.apply1[AA](name + " = " + _)(a))

  def assignToNewVar[AA >: A](varName: String)(implicit e: ExprParam[AA], l: Language): Expr[AA] =
    flatTap(a => Expr.apply1[AA]("var " + varName + " = " + _)(a))

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

  def asBooleanArray(implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Boolean]] = _as(_.toBooleanArray())
  def asByteArray   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Byte   ]] = _as(_.toByteArray())
  def asDoubleArray (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Double ]] = _as(_.toDoubleArray())
  def asFloatArray  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Float  ]] = _as(_.toFloatArray())
  def asIntArray    (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Int    ]] = _as(_.toIntArray())
  def asLongArray   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Long   ]] = _as(_.toLongArray())
  def asShortArray  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[Short  ]] = _as(_.toShortArray())
  def asStringArray (implicit ev: Expr[A] <:< Expr[Value]): Expr[Array[String ]] = _as(_.toStringArray())

  def asBooleanList(implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Boolean]] = _as(_.toBooleanList())
  def asByteList   (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Byte   ]] = _as(_.toByteList())
  def asDoubleList (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Double ]] = _as(_.toDoubleList())
  def asFloatList  (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Float  ]] = _as(_.toFloatList())
  def asIntList    (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Int    ]] = _as(_.toIntList())
  def asLongList   (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Long   ]] = _as(_.toLongList())
  def asShortList  (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[Short  ]] = _as(_.toShortList())
  def asStringList (implicit ev: Expr[A] <:< Expr[Value]): Expr[List[String ]] = _as(_.toStringList())

  def asBooleanVector(implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Boolean]] = _as(_.toBooleanVector())
  def asByteVector   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Byte   ]] = _as(_.toByteVector())
  def asDoubleVector (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Double ]] = _as(_.toDoubleVector())
  def asFloatVector  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Float  ]] = _as(_.toFloatVector())
  def asIntVector    (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Int    ]] = _as(_.toIntVector())
  def asLongVector   (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Long   ]] = _as(_.toLongVector())
  def asShortVector  (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[Short  ]] = _as(_.toShortVector())
  def asStringVector (implicit ev: Expr[A] <:< Expr[Value]): Expr[Vector[String ]] = _as(_.toStringVector())

  def void: Expr[Unit] =
    new Expr(c => {run(c); ()})

  def asOption[B](f: Expr[Value] => Expr[B])(implicit ev: Expr[A] <:< Expr[Value]): Expr[Option[B]] = {
    val self = ev(this)
    new Expr(c => {
      val v = self.run(c)
      if (ExprError.InResult.capture(v, _.isNull))
        None
      else
        Some(f(Expr.pure(v)).run(c))
    })
  }

  def asPromise(implicit ev: Expr[A] <:< Expr[Value],
                l: Language,
                pc: ExprParam[Consumer[AnyRef]],
                pv: ExprParam[Value]): Expr[Future[Value]] =
    asPromise[Value](identity)

  def asPromise[B](f: Value => B)(implicit ev: Expr[A] <:< Expr[Value],
                                  l: Language,
                                  pc: ExprParam[Consumer[AnyRef]],
                                  pv: ExprParam[Value]): Expr[Future[B]] = {
    for {
      v     <- ev(this)
      p      = Promise[B]()
      onThen = (v => p.complete(Try(f(Value.asValue(v)))))                       : Consumer[AnyRef]
      onFail = (v => p.failure(ExprError.AsyncFunctionFailed(Value.asValue(v)))) : Consumer[AnyRef]
      _     <- Expr.apply3[Value, Consumer[AnyRef], Consumer[AnyRef]]((a, b, c) => s"$a.then($b).catch($c)")(v, onThen, onFail)
    } yield p.future
  }

  def await[B](implicit ev: Expr[A] <:< Expr[Future[B]]): Expr[B] =
    await(Expr.DefaultAwaitTimeout)

  def await[B](atMost: Duration)(implicit ev: Expr[A] <:< Expr[Future[B]]): Expr[B] =
    ev(this).map(Await.result(_, atMost))

  def awaitAttempt[B](implicit ev: Expr[A] <:< Expr[Future[B]]): Expr[Either[Future[B], B]] =
    awaitAttempt(Expr.DefaultAwaitTimeout)

  def awaitAttempt[B](atMost: Duration)(implicit ev: Expr[A] <:< Expr[Future[B]]): Expr[Either[Future[B], B]] =
    ev(this).map { f =>
      try
        Right(Await.result(f, atMost))
      catch {
        case _: TimeoutException => Left(f)
      }
    }

  def >>[B](next: Expr[B]): Expr[B] =
    new Expr(c => {run(c); next.run(c)})

  @inline def <<[B](prev: Expr[B]): Expr[A] =
    prev >> this

  def timed: Expr[(A, DurationLite)] =
    new Expr(ctx => {
      val timer = DurationLite.start()
      val a = run(ctx)
      val dur = timer.stop()
      (a, dur)
    })
}

object Expr extends ExprBoilerplate {
  type Result[+A] = Either[ExprError, A]

  val DefaultAwaitTimeout = Duration(10, SECONDS)

  def apply(source: CharSequence)(implicit language: Language): Expr[Value] =
    apply(Source.create(language.name, source))

  def apply(source: => Source): Expr[Value] = {
    lazy val s = source
    new Expr(c => ExprError.InEval.capture(c.eval(s)))
  }

  def lift[A](f: Context => A): Expr[A] =
    new Expr(f)

  def pure[A](a: A): Expr[A] =
    new Expr(_ => a)

  def point[A](a: => A): Expr[A] =
    new Expr(_ => a)

  def lazily[A](a: => A): Expr[A] = {
    lazy val l = a
    new Expr(_ => l)
  }

  val unit: Expr[Unit] =
    pure(())

  def fail[A](e: ExprError): Expr[A] =
    point(throw e)

  def suspend[A](e: => Expr[A]): Expr[A] =
    new Expr(c => e.run(c))

  def tailrec[A, B](init: => A)(f: A => Expr[Either[A, B]]): Expr[B] =
    lift[B] { ctx =>
      @tailrec def go(a1: A): B =
        f(a1).run(ctx) match {
          case Left(a2) => go(a2)
          case Right(b) => b
        }
      go(init)
    }

  def runAll(es: Expr[Any]*): Expr[Unit] =
    cosequenceAndDiscard(es)

  def distribute[F[x] <: Iterable[x], A, B](fa: F[A])(f: A => Expr[B])
                                           (implicit cbf: BuildFrom[F[A], B, F[B]]): Expr[F[B]] =
    lift(c => {
      val b = cbf.newBuilder(fa)
      fa.foreach(a => b += f(a).run(c))
      b.result()
    })

  def cosequence[F[x] <: Iterable[x], A](fea: F[Expr[A]])
                                        (implicit cbf: BuildFrom[F[Expr[A]], A, F[A]]): Expr[F[A]] =
    distribute[F, Expr[A], A](fea)(identity)

  def distributeAndDiscard[A, B](fa: Iterable[A])(f: A => Expr[B]): Expr[Unit] =
    lift(c => fa.foreach(f(_).run(c)))

  def cosequenceAndDiscard[A](es: Iterable[Expr[A]]): Expr[Unit] =
    lift(c => es.foreach(_.run(c)))

  def fileOnClasspath(filename: String, srcCfg: Source#Builder => Source#Builder = identity)(implicit lang: Language): Expr[Option[Value]] =
    lazily(GraalSourceUtil.fileOnClasspath(lang.name, filename, srcCfg)).flatMap {
      case Some(s) => apply(s).map(Some(_))
      case None    => pure(None)
    }

  def requireFileOnClasspath(filename: String, srcCfg: Source#Builder => Source#Builder = identity)(implicit lang: Language): Expr[Value] =
    apply(GraalSourceUtil.requireFileOnClasspath(lang.name, filename, srcCfg))

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
