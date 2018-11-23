package japgolly.scalagraal

import org.graalvm.polyglot.{Context, Engine}

trait ContextSync {

  def eval[A](expr: Expr[A],
              additionalMetricWriter: ContextMetrics.Writer = ContextMetrics.Writer.Noop
             ): Expr.Result[A] =
    evalT(expr, DurationLite.start(), additionalMetricWriter)

  def evalWithStats[A](expr: Expr[A],
                       additionalMetricWriter: ContextMetrics.Writer = ContextMetrics.Writer.Noop
                      ): (Expr.Result[A], ContextMetrics.Stats) = {
    val v = ContextMetrics.Writer.StoreLast()
    val r = evalT(expr, DurationLite.start(), additionalMetricWriter >> v)
    (r, v.last)
  }

  def close(): Unit

  private[scalagraal] def evalT[A](expr: Expr[A],
                                   startTime: DurationLite.StartTime,
                                   metricWriter2: ContextMetrics.Writer): Expr.Result[A]
}

object ContextSync {

  def apply()(implicit l: Language): ContextSync = fixedContext()

  def fixedContext()(implicit l: Language): ContextSync = Builder.fixedContext().build()
  def fixedContext(ls: Seq[Language]): ContextSync = Builder.fixedContext(ls).build()
  def fixedContext(c: Context): ContextSync = Builder.fixedContext(c).build()

  def newContextPerUse()(implicit l: Language): ContextSync = Builder.newContextPerUse().build()
  def newContextPerUse(ls: Seq[Language]): ContextSync = Builder.newContextPerUse(ls).build()
  def newContextPerUse(c: => Context): ContextSync = Builder.newContextPerUse(c).build()
  def newContextPerUse(f: Engine => Context): ContextSync = Builder.newContextPerUse(f).build()

  object Builder {

    def fixedContext()(implicit l: Language): Builder =
      fixedContext(l :: Nil)

    def fixedContext(ls: Seq[Language]): Builder =
      fixedContext(Context.create(ls.map(_.name): _*))

    def fixedContext(c: Context): Builder =
      start(Right(c), useMutex = true)

    def newContextPerUse()(implicit l: Language): Builder =
      newContextPerUse(l :: Nil)

    def newContextPerUse(ls: Seq[Language]): Builder =
      newContextPerUse(Context.newBuilder(ls.map(_.name): _*).engine(_).build())

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
                      _afterCreate: Option[Expr[Any]],
                      _beforeEval: Option[Expr[Any]],
                      _afterEval: Option[Expr[Any]],
                      _beforeClose: Option[Expr[Any]],
                      _metricWriter: Option[ContextMetrics.Writer]) {

    private def copy(_ctxProvider: Either[() => Context, Context] = _ctxProvider,
                     _useMutex: Boolean = _useMutex,
                     _afterCreate: Option[Expr[Any]] = _afterCreate,
                     _beforeEval: Option[Expr[Any]] = _beforeEval,
                     _afterEval: Option[Expr[Any]] = _afterEval,
                      _beforeClose: Option[Expr[Any]] = _beforeClose,
                     _metricWriter: Option[ContextMetrics.Writer] = _metricWriter): Builder =
      new Builder(_ctxProvider, _useMutex, _afterCreate, _beforeEval, _afterEval, _beforeClose, _metricWriter)

    def useMutex(b: Boolean): Builder =
      copy(_useMutex = b)

    def onContextCreate(e: Expr[Any]): Builder =
      copy(_afterCreate = Some(_afterCreate.fold[Expr[Any]](e)(_ >> e)))

    def beforeEval(e: Expr[Any]): Builder =
      copy(_beforeEval = Some(_beforeEval.fold[Expr[Any]](e)(_ >> e)))

    def afterEval(e: Expr[Any]): Builder =
      copy(_afterEval = Some(_afterEval.fold[Expr[Any]](e)(_ >> e)))

    def onContextClose(e: Expr[Any]): Builder =
      copy(_beforeClose = Some(_beforeClose.fold[Expr[Any]](e)(_ >> e)))

    def writeMetrics(w: ContextMetrics.Writer): Builder =
      copy(_metricWriter = Some(_metricWriter.fold(w)(_ >> w)))

    def build(): ContextSync = {
      def append(a: Option[Expr[Any]], b: Option[Expr[Any]]): Expr[Any] =
        (a, b) match {
          case (Some(x), Some(y)) => x >> y
          case (Some(x), None   ) => x
          case (None   , Some(y)) => y
          case (None   , None   ) => Expr.unit
        }

      val metricWriter = _metricWriter.getOrElse(ContextMetrics.Writer.Noop)

      _ctxProvider match {

        case Left(newCtx) =>
          // new context per eval
          new Impl(
            useMutex = _useMutex,
            getCtx = newCtx,
            beforeEval = append(_afterCreate, _beforeEval),
            afterEval = append(_afterEval, _beforeClose),
            closeCtx = Builder.close,
            metricWriter = metricWriter,
            onClose = () => ())

        case Right(fixedCtx) =>
          def evalOrThrow(e: Expr[Any]): Unit = {
            fixedCtx.enter()
            try e.evalOrThrow(fixedCtx) finally fixedCtx.leave()
            ()
          }
          _afterCreate.foreach(evalOrThrow)
          new Impl(
            useMutex = _useMutex,
            getCtx = () => fixedCtx,
            beforeEval = _beforeEval.getOrElse(Expr.unit),
            afterEval = _afterEval.getOrElse(Expr.unit),
            closeCtx = Builder.dontClose,
            metricWriter = metricWriter,
            onClose = () => try _beforeClose.foreach(evalOrThrow) finally fixedCtx.close())
      }
    }
  }

  private final class Impl(useMutex: Boolean,
                           getCtx: () => Context,
                           beforeEval: Expr[Any],
                           afterEval: Expr[Any],
                           closeCtx: Context => Unit,
                           metricWriter: ContextMetrics.Writer,
                           onClose: () => Unit) extends ContextSync {

    private[this] val evalLock: AnyRef =
      if (useMutex) new AnyRef else null

    override private[scalagraal] def evalT[A](expr: Expr[A],
                                              timerTotal: DurationLite.StartTime,
                                              metricWriter2: ContextMetrics.Writer): Expr.Result[A] = {
      // We should really check here if were closed but...
      // 1. That would require making closed volatile (or using a lock; yuk)
      // 2. Fixed contexts will throw an exception anyway when closed.
      // 3. NewCtxPerEval will still work after closed but it closed itself and it can be argued close != pool.shutdown
      // 4. The reason I'm adding close is literally only for the fixed context case
      var durWaited, durPre, durBody, durPost = DurationLite.Zero
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
                  val timerBody = DurationLite.start()
                  val result = expr(ctx)
                  durBody = timerBody.stop()
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
          if (evalLock eq null) resultFn() else evalLock.synchronized(resultFn())

        if ((afterEvalResult ne null) && afterEvalResult.isLeft && result.isRight)
          afterEvalResult.asInstanceOf[Expr.Result[A]]
        else
          result

      } finally {
        val mw = metricWriter2 >> metricWriter
        val durTotal = timerTotal.stop()
        val stats = ContextMetrics.Stats(
          waited = durWaited,
          pre    = durPre,
          body   = durBody,
          post   = durPost,
          total  = durTotal)
        mw(stats)
      }
    }

    private[this] val closeLock = new AnyRef
    private[this] var closed = false

    override def close(): Unit =
      closeLock.synchronized {
        if (!closed) {
          closed = true
          onClose()
        }
      }

  }
}

