package japgolly.scalagraal

import java.time.Duration
import org.graalvm.polyglot._
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

final class Expr[A] private[Expr] (private[Expr] val run: Context => A) extends AbstractFunction1[Context, Expr.Result[A]] {

  override def apply(context: Context): Expr.Result[A] =
    try {
      Right(run(context))
    } catch {
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

  final class Interpolation(private val sc: StringContext) extends AnyVal {

    def js(args: Any*): Expr[Value] =
      build(Language.JS, args: _*)

    private def build(lang: Language, args: Any*): Expr[Value] =
      if (args.isEmpty)
        Expr(sc.parts.head)(lang)
      else {
        val argArray: Array[Any] = args.map(lang.translateValue)(collection.breakOut)
        val iParts = sc.parts.iterator
        var i = 0
        val sb = new StringBuilder(iParts.next())
        while (iParts.hasNext) {
          sb.append(lang.scalaGraalArgB.localValue)
          sb.append('[')
          sb.append(i)
          sb.append(']')
          sb.append(iParts.next)
          i += 1
        }
        val body = sb.toString()
        val bodySrc = Source.create(lang.name, body)
        val eval = lang.scalaGraalArgF(bodySrc)
        lift{ctx =>
          ctx.getPolyglotBindings.putMember(lang.scalaGraalArgB.bindingName, argArray)
          eval(ctx)
        }
      }

  }

  // TODO Should be per language
  final case class Arg[A](prepare: A => ArgValue) extends AnyVal
  object Arg {
    def const[A](v: ArgValue): Arg[A] = Arg(_ => v)
  }

  sealed trait ArgValue
  object ArgValue {
    final case class Literal(expr: String) extends ArgValue
    final case class Polyglot(expr: Any) extends ArgValue
    final case class Custom(expr: Context => Any) extends ArgValue
  }

  trait CommonArgs {
    //implicit val jsArgInt: Arg[Int] = Arg(i => ArgValue.Literal(i.toString))
    implicit val jsArgInt: Arg[Int] = Arg(ArgValue.Polyglot)
    implicit val jsArgLong: Arg[Long] = Arg(ArgValue.Polyglot)
    implicit val jsArgString: Arg[String] = Arg(ArgValue.Polyglot)
    implicit val jsArgBoolean: Arg[Boolean] = Arg(ArgValue.Polyglot)
    implicit val jsArgShort: Arg[Short] = Arg(ArgValue.Polyglot)
    implicit val jsArgFloat: Arg[Float] = Arg(ArgValue.Polyglot)
    implicit val jsArgDouble: Arg[Double] = Arg(ArgValue.Polyglot)
  }

  trait JsArgs {
    implicit val jsArgUnit: Arg[Unit] = Arg.const(ArgValue.Literal("undefined"))
  }

  final class MutableBuilder(size: Int) {
    val array = new Array[Any](size)
    var lateEvals = List.empty[Context => Unit]
    var used = false

    def add(i: Int, av: ArgValue)(implicit l: Language): String =
      av match {
        case ArgValue.Literal(t) =>
          t
        case ArgValue.Polyglot(a) =>
          used = true
          array(i) = a
          l.blah22(i)
        case ArgValue.Custom(f) =>
          used = true
          lateEvals ::= (ctx => array(i) = f(ctx))
          l.blah22(i)
      }
  }

  def args2[A, B](f: (String, String) => String, a: A, b: B)(implicit fa: Arg[A], fb: Arg[B], l: Language): Expr[Value] = {
    val builder = new MutableBuilder(2)
    val aa = builder.add(0, fa.prepare(a))
    val ab = builder.add(1, fb.prepare(b))
    val bodyStr = f(aa, ab)
    val bodySrc = Source.create(l.name, bodyStr)
    if (builder.used) {
      val eval = l.scalaGraalArgF(bodySrc)
      val lateEvals = builder.lateEvals
      val array = builder.array
      lift { ctx =>
        if (lateEvals.nonEmpty) lateEvals.foreach(_(ctx))
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, array)
        eval(ctx)
      }
    } else {
      Expr(bodySrc)
    }
  }

  def fn1[A](mkExpr: String => String)(implicit argA: Arg2[A], l: Language): A => Expr[Value] = {
    val builder = new MutableBuilder2(2)
    val ra = builder.add(argA)

    ra match {
      case Right((None, argStr)) =>
        val exprStr = mkExpr(argStr)
        val exprSrc = Source.create(l.name, exprStr)
        val expr = Expr(exprSrc)
        _ => expr
      case Right((Some(argSet), argStr)) =>
        val exprStr = mkExpr(argStr)
        val exprSrc = Source.create(l.name, exprStr)
        builder.onCtx match {
          case None =>
            a => lift { ctx =>
              argSet(a)
              ctx.eval(exprSrc)
            }
        }

    }

//    val bodyStr = f(aa, ab)
//    val bodySrc = Source.create(l.name, bodyStr)
//    if (builder.used) {
//      val eval = l.scalaGraalArgF(bodySrc)
//      val lateEvals = builder.lateEvals
//      val array = builder.array
//      lift { ctx =>
//        if (lateEvals.nonEmpty) lateEvals.foreach(_(ctx))
//        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, array)
//        eval(ctx)
//      }
//    } else {
//      Expr(bodySrc)
//    }
  }

  sealed trait Arg2[A]
  object Arg2 {
    final case class Const[A](expr: String) extends Arg2[A]
    final case class Literal[A](expr: A => String) extends Arg2[A]
    final case class Polyglot[A](mkValue: A => Any) extends Arg2[A]
    final case class Custom[A](mkValue: A => Context => Any) extends Arg2[A]

    final class AndValue[A](val arg: Arg[A], val value: A)
  }

  /*
  Const => {expr;    _ => expr}
  Lit   =>           a => {expr(src(f(a)) }
  Pol   => {expr;    a => {new Array(a); expr}}
  Cust  => {exprSrc; a => {new Array(); f=(a); Expr(ctx => {f(ctx); exprSrc}}}
   */

  def fromConst[A](g: Arg2.Const[A], f: String => String)(implicit l: Language): A => Expr[Value] = {
    val e = Expr(f(g.expr))
    _ => e
  }
  def fromLiteral[A](g: Arg2.Literal[A], f: String => String)(implicit l: Language): A => Expr[Value] = {
    // nothing built up front here
    a => Expr(f(g.expr(a)))
  }
  def fromPolyglot[A](g: Arg2.Polyglot[A], f: String => String)(implicit l: Language): A => Expr[Value] = {
    val run = l.scalaGraalArgF(Source.create(l.name, f(l.blah22(0))))
    a => {
      val d = new Array[Any](1)
      d(0) = g.mkValue(a)
      lift { ctx =>
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }
  def fromCustom[A](g: Arg2.Custom[A], f: String => String)(implicit l: Language): A => Expr[Value] = {
    val run = l.scalaGraalArgF(Source.create(l.name, f(l.blah22(0))))
    a => {
      val h = g.mkValue(a)
      lift { ctx =>
        val d = new Array[Any](1)
        d(0) = h(ctx)
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }
  def fromLiteralPolyglot[A, B](ga: Arg2.Literal[A], gb: Arg2.Polyglot[B], f: (String, String) => String)(implicit l: Language): (A, B) => Expr[Value] = {
    // nothing built up front here
    (a, b) => {
      val ea = ga.expr(a)
      val run = l.scalaGraalArgF(Source.create(l.name, f(ea, l.blah22(1))))
      val d = new Array[Any](2)
      d(1) = gb.mkValue(b)
      lift { ctx =>
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }

  def all[A, B, C, D](ga: Arg2.Const[A],
                      gb: Arg2.Literal[B],
                      gc: Arg2.Polyglot[C],
                      gd: Arg2.Custom[D],
                      f: (String, String, String, String) => String)(implicit l: Language): (A, B, C, D) => Expr[Value] = {
    // nothing built up front here
    (_, b, c, d) => {
      val ea = ga.expr
      val eb = gb.expr(b)
      val vc = gc.mkValue(c)
      val fd = gd.mkValue(d)
      val es = f(ea, eb, l.blah22(2), l.blah22(3))
      val run = l.scalaGraalArgF(Source.create(l.name, es))
      lift { ctx =>
        val d = new Array[Any](4)
        // d(0) = const
        // d(1) = literal
        d(2) = vc
        d(3) = fd(ctx)
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }

  def nonLiteral[A, C, D](ga: Arg2.Const[A],
                          gc: Arg2.Polyglot[C],
                          gd: Arg2.Custom[D],
                          f: (String, String, String) => String)(implicit l: Language): (A, C, D) => Expr[Value] = {
    val ea = ga.expr
    val es = f(ea, l.blah22(2), l.blah22(3))
    val run = l.scalaGraalArgF(Source.create(l.name, es))
    (_, c, d) => {
      val vc = gc.mkValue(c)
      val fd = gd.mkValue(d)
      lift { ctx =>
        val d = new Array[Any](4)
        // d(0) = const
        // d(1) = literal
        d(2) = vc
        d(3) = fd(ctx)
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }

  private sealed trait AA
  private def genericAll(params: Array[Arg2[AA]], mkExpr: Array[String] => String)(implicit l: Language): Array[AA] => Expr[Value] = {
    val arity = params.length
    val indices = params.indices
    args => {
      var setValues = List.empty[(Array[Any], Context) => Unit]

      val run = {
        val tokens = new Array[String](arity)
        for (i <- indices) {
          val arg = args(i)
          val token: String = params(i) match {
            case Arg2.Const(t) => t
            case Arg2.Literal(f) => f(arg)
            case Arg2.Polyglot(f) =>
              val v = f(arg)
              setValues ::= ((tgt, _) => tgt(i) = v)
              l.blah22(i)
            case Arg2.Custom(f) =>
              val g = f(arg)
              setValues ::= ((tgt, ctx) => tgt(i) = g(ctx))
              l.blah22(i)
          }
          tokens(i) = token
        }
        val es = mkExpr(tokens)
        l.scalaGraalArgF(Source.create(l.name, es))
      }

      lift { ctx =>
        val d = new Array[Any](arity)
        setValues.foreach(_(d, ctx))
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }
  private def genericNonLiterals(params: Array[Arg2[AA]], mkExpr: Array[String] => String)(implicit l: Language): Array[AA] => Expr[Value] = {
    val arity = params.length
    val indices = params.indices

    val run = {
      val tokens = new Array[String](arity)
      for (i <- indices) {
        val token: String = params(i) match {
          case Arg2.Const(t) => t
          case Arg2.Literal(_) => ???
          case Arg2.Polyglot(_) => l.blah22(i)
          case Arg2.Custom(_) => l.blah22(i)
        }
        tokens(i) = token
      }
      val es = mkExpr(tokens)
      l.scalaGraalArgF(Source.create(l.name, es))
    }

    args => {
      var setValues = List.empty[(Array[Any], Context) => Unit]

        for (i <- indices) {
          val arg = args(i)
          params(i) match {
            case Arg2.Const(_) => ()
            case Arg2.Literal(_) => ()
            case Arg2.Polyglot(f) =>
              val v = f(arg)
              setValues ::= ((tgt, _) => tgt(i) = v)
            case Arg2.Custom(f) =>
              val g = f(arg)
              setValues ::= ((tgt, ctx) => tgt(i) = g(ctx))
          }
        }

      lift { ctx =>
        val d = new Array[Any](arity)
        setValues.foreach(_(d, ctx))
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }
  private def genericPreOpt(params: Array[Arg2[AA]], mkExpr: Array[String] => String)(implicit l: Language): Array[AA] => Expr[Value] = {
    val arity = params.length
    val indices = params.indices

    def mkRun(args: Array[AA]): Context => Value = {
      val tokens = new Array[String](arity)
      for (i <- indices) {
        val token: String = params(i) match {
          case Arg2.Const(t) => t
          case Arg2.Polyglot(_) => l.blah22(i)
          case Arg2.Custom(_) => l.blah22(i)
          case Arg2.Literal(f) => f(args(i))
        }
        tokens(i) = token
      }
      val es = mkExpr(tokens)
      l.scalaGraalArgF(Source.create(l.name, es))
    }

    def mkSetValues(args: Array[AA]): List[(Array[Any], Context) => Unit] = {
      var setValues = List.empty[(Array[Any], Context) => Unit]
      for (i <- indices) {
        params(i) match {
          case Arg2.Polyglot(f) =>
            val v = f(args(i))
            setValues ::= ((tgt, _) => tgt(i) = v)
          case Arg2.Custom(f) =>
            val g = f(args(i))
            setValues ::= ((tgt, ctx) => tgt(i) = g(ctx))
          case Arg2.Const(_) | Arg2.Literal(_) => ()
        }
      }
      setValues
    }

    args => {
      val run = mkRun(args)
      val setValues = mkSetValues(args)
      lift { ctx =>
        val d = new Array[Any](arity)
        setValues.foreach(_(d, ctx))
        ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
        run(ctx)
      }
    }
  }
  private def genericOpt(params: Array[Arg2[AA]], mkExprStr: Array[String] => String)(implicit l: Language): Array[AA] => Expr[Value] = {
    val arity = params.length
    val indices = params.indices

    // !Literal
    def mkRun(args: Array[AA]): Context => Value = {
      val tokens = new Array[String](arity)
      for (i <- indices) {
        val token: String = params(i) match {
          case Arg2.Const(t) => t
          case Arg2.Polyglot(_) => l.blah22(i)
          case Arg2.Custom(_) => l.blah22(i)
          case Arg2.Literal(f) => f(args(i))
        }
        tokens(i) = token
      }
      val es = mkExprStr(tokens)
      l.scalaGraalArgF(Source.create(l.name, es))
    }

    def mkArrayP(args: Array[AA]): Array[Any] = {
      val d = new Array[Any](arity)
      for (i <- indices) {
        params(i) match {
          case Arg2.Polyglot(f) => d(i) = f(args(i))
          case Arg2.Custom(_) | Arg2.Const(_) | Arg2.Literal(_) => ()
        }
      }
      d
    }

    def mkSetValuesPC(args: Array[AA]): List[(Array[Any], Context) => Unit] = {
      var setValues = List.empty[(Array[Any], Context) => Unit]
      for (i <- indices) {
        params(i) match {
          case Arg2.Polyglot(f) =>
            val v = f(args(i))
            setValues ::= ((tgt, _) => tgt(i) = v)
          case Arg2.Custom(f) =>
            val g = f(args(i))
            setValues ::= ((tgt, ctx) => tgt(i) = g(ctx))
          case Arg2.Const(_) | Arg2.Literal(_) => ()
        }
      }
      setValues
    }

    def mkExprWithBindings(run: Context => Value, args: Array[AA], hasCustom: Boolean) =
      if (hasCustom) {
        val setValues = mkSetValuesPC(args)
        lift { ctx =>
          val d = new Array[Any](arity)
          setValues.foreach(_ (d, ctx))
          ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
          run(ctx)
        }
      } else {
        val d = mkArrayP(args)
        lift { ctx =>
          ctx.getPolyglotBindings.putMember(l.scalaGraalArgB.bindingName, d)
          run(ctx)
        }
      }

    var hasConst, hasLiteral, hasPolyglot, hasCustom = false
    params.foreach {
      case Arg2.Const(_) => hasConst = true
      case Arg2.Literal(_) =>  hasLiteral = true
      case Arg2.Polyglot(_) =>  hasPolyglot = true
      case Arg2.Custom(_) =>  hasCustom = true
    }


    val usesBindings = hasPolyglot || hasCustom
    (hasLiteral, usesBindings) match {

      case (false, false) =>
        val expr = lift(mkRun(null))
        _ => expr

      case (true, false) =>
        args => lift(mkRun(args))

      case (false, true) =>
        val run = mkRun(null)
        args => mkExprWithBindings(run, args, hasCustom = hasCustom)

      case (true, true) =>
        args => {
          val run = mkRun(args)
          mkExprWithBindings(run, args, hasCustom = hasCustom)
        }
    }
  }


  // precompilation = !literal
  // bindings       = polyglot | custom
  // run def        = C:outside, L:inside

  final class MutableBuilder2(size: Int) {
    private var i = -1
    private val argArray = new Array[Any](size)

    private var ctxSets = 0
    private val ctxArray = new Array[Context => Unit](size)

    def onCtx: Option[Context => Unit] =
      if (ctxSets == 0)
        None
      else
        Some(ctx => {
          var j = ctxSets
          while (j > 0) {
            j -= 1
            ctxArray(j)(ctx)
          }
        })

    def add[A](arg: Arg2[A])(implicit l: Language): Either[A => String, (Option[A => Unit], String)] = {
      i += 1
      arg match {
        case Arg2.Const(t) =>
          Right(None -> t)
        case Arg2.Literal(f) =>
          Left(f)
        case Arg2.Polyglot(f) =>
          Right(Some(a => argArray(i) = a), l.blah22(i))
        case Arg2.Custom(f) =>
          val c = ctxSets
          ctxSets += 1
          val argFn: A => Unit = a => {
            val ctxToValue: Context => Any = f(a)
            ctxArray(c) = ctx => argArray(i) = ctxToValue(ctx)
          }
          Right(Some(argFn), l.blah22(i))
      }
    }
  }

}
