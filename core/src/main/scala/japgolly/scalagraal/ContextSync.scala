package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Engine}

trait ContextSync {
  def eval[A](f: Expr[A]): Expr.Result[A]
}

object ContextSync {

  def apply()(implicit l: Language): ContextSync = fixedContext()

  def fixedContext()(implicit l: Language): ContextSync = Builder.fixedContext().build()
  def fixedContext(c: Context): ContextSync = Builder.fixedContext(c).build()
  def newContextPerUse()(implicit l: Language): ContextSync = Builder.newContextPerUse().build()
  def newContextPerUse(c: => Context): ContextSync = Builder.newContextPerUse(c).build()
  def newContextPerUse(f: Engine => Context): ContextSync = Builder.newContextPerUse(f).build()

  object Builder {

    def fixedContext()(implicit l: Language): Builder =
      fixedContext(Context.create(l.name))

    def fixedContext(c: Context): Builder =
      start(Right(c), useMutex = true)

    def newContextPerUse()(implicit l: Language): Builder =
      newContextPerUse(Context.newBuilder(l.name).engine(_).build())

    def newContextPerUse(c: => Context): Builder =
      start(Left(() => c), useMutex = false)

    def newContextPerUse(f: Engine => Context): Builder = {
      val e = Engine.create()
      start(Left(() => f(e)), useMutex = false)
    }

    private def start(c: Either[() => Context, Context], useMutex: Boolean): Builder =
      new Builder(c, _useMutex = useMutex, None, None, None, None, None)

    private[Builder] val close: Context => Unit = _.close(true)
    private[Builder] val dontClose: Context => Unit = _ => ()
  }

  final class Builder(_ctxProvider: Either[() => Context, Context],
                      _useMutex: Boolean,
                      _afterCreate: Option[Expr[_]],
                      _beforeEval: Option[Expr[_]],
                      _afterEval: Option[Expr[_]],
                      _preClose: Option[Expr[_]],
                      _metricWriter: Option[ContextMetrics.Writer]) {
    //                      _metricsDims: List[Metrics.Dimension]) {

    private def copy(_ctxProvider: Either[() => Context, Context] = _ctxProvider,
                     _useMutex: Boolean = _useMutex,
                     _afterCreate: Option[Expr[_]] = _afterCreate,
                     _beforeEval: Option[Expr[_]] = _beforeEval,
                     _afterEval: Option[Expr[_]] = _afterEval,
                      _preClose: Option[Expr[_]] = _preClose,
                     _metricWriter: Option[ContextMetrics.Writer] = _metricWriter): Builder =
      new Builder(_ctxProvider, _useMutex, _afterCreate, _beforeEval, _afterEval, _preClose, _metricWriter)

    def useMutex(b: Boolean): Builder =
      copy(_useMutex = b)

    def afterContextCreate(e: Expr[_]): Builder =
      copy(_afterCreate = Some(_afterCreate.fold[Expr[_]](e)(_ >> e)))

    def beforeEval(e: Expr[_]): Builder =
      copy(_beforeEval = Some(_beforeEval.fold[Expr[_]](e)(_ >> e)))

    def afterEval(e: Expr[_]): Builder =
      copy(_afterEval = Some(_afterEval.fold[Expr[_]](e)(_ >> e)))

    def beforeContextClose(e: Expr[_]): Builder =
      copy(_preClose = Some(_preClose.fold[Expr[_]](e)(_ >> e)))

    def writeMetrics(w: ContextMetrics.Writer): Builder =
      copy(_metricWriter = Some(_metricWriter.fold(w)(_ >> w)))

    def build(): ContextSync =
      new Impl(
        useMutex = _useMutex,
        getCtx = _ctxProvider.fold(identity, () => _),
        beforeEval = _beforeEval.getOrElse(Expr.unit),
        afterEval = _afterEval.getOrElse(Expr.unit),
        closeCtx = if (_ctxProvider.isLeft) Builder.close else Builder.dontClose,
        metricWriter = _metricWriter.getOrElse(ContextMetrics.Noop))
  }

  private final class Impl(useMutex: Boolean,
                           getCtx: () => Context,
                           beforeEval: Expr[_],
                           afterEval: Expr[_],
                           closeCtx: Context => Unit,
                           metricWriter: ContextMetrics.Writer) extends ContextSync {

    private[this] val lock: AnyRef =
      if (useMutex) new AnyRef else null

    override def eval[A](expr: Expr[A]): Expr.Result[A] = {
      val timerTotal = DurationLite.start()
      var durWaited, durPre, durEval, durPost = DurationLite.Zero
      var afterEvalResult: Expr.Result[_] = null
      try {
        val resultFn = () => {
          durWaited = timerTotal.stop()
          // ----------------------------------------------------
          val timerPre = DurationLite.start()
          var timerPost = timerPre // this will be set before used
          val ctx = getCtx()
          try {
            ctx.enter()
            try {
              val resultBefore = beforeEval(ctx)
              durPre = timerPre.stop()
              if (resultBefore.isLeft) {
                resultBefore.asInstanceOf[Expr.Result[A]]
              } else {
                // ----------------------------------------------------
                try {
                  val timerEval = DurationLite.start()
                  val result = expr(ctx)
                  durEval = timerEval.stop()
                  result
                  // ----------------------------------------------------
                } finally {
                  timerPost = DurationLite.start()
                  afterEvalResult = afterEval(ctx)
                }
              }

            } finally {
              ctx.leave()
            }
          } finally {
            closeCtx(ctx)
            durPost = timerPost.stop()
          }
          // ----------------------------------------------------
        }

        val result =
          if (lock eq null) resultFn() else lock.synchronized(resultFn())

        if ((afterEvalResult ne null) && afterEvalResult.isLeft && result.isRight)
          afterEvalResult.asInstanceOf[Expr.Result[A]]
        else
          result

      } finally {
        val durTotal = timerTotal.stop()
        metricWriter(
          waited = durWaited,
          pre = durPre,
          eval = durEval,
          post = durPost,
          total = durTotal)
      }
    }
  }
}

