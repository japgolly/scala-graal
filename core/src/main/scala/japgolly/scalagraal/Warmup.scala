package japgolly.scalagraal

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

object Warmup {

  final case class State(outerReps: Int,
                         totalInnerReps: Int,
                         repEvalTimes: Vector[DurationLite],
                         totalWarmupTime: DurationLite) {
    override def toString =
      s"Warmup.State(outerReps = $outerReps, totalInnerReps = $totalInnerReps, repEvalTimes = [${repEvalTimes.head}, …, ${repEvalTimes.last}], totalWarmupTime = ${totalWarmupTime.toStrSec})"

    def firstEvalInLastRep: DurationLite = repEvalTimes.head
    def lastEval: DurationLite = repEvalTimes.last
    def lastEvals(size: Int): Vector[DurationLite] = repEvalTimes.takeRight(size)
    def lastEvalAverage(size: Int): DurationLite = lastEvals(size).reduce(_ + _) / size
  }

  object State {
    def empty = State(0, 0, Vector.empty, DurationLite.Zero)
  }

  def sync(ctx: ContextSync)
          (innerReps: Int,
           expr: Expr[Any],
           stopWhen: State => Boolean = _ => true): State = {

    val warmupStart = DurationLite.start()

    val innerExpr = mkInnerExpr(innerReps, expr)

    @tailrec def go(s: State): State =
      if (s.outerReps != 0 && stopWhen(s))
        s
      else {
        val result = ctx.eval(innerExpr).fold(throw _, identity)
        go(State(
          outerReps = s.outerReps + 1,
          totalInnerReps = s.totalInnerReps + innerReps,
          repEvalTimes = result,
          totalWarmupTime = warmupStart.stop()
        ))
      }

    go(State.empty)
  }

  private def mkInnerExpr(innerReps: Int, expr: Expr[Any]): Expr[Vector[DurationLite]] = Expr.lift { ctx =>
    val b = Vector.newBuilder[DurationLite]
    b.sizeHint(innerReps)
    var i = innerReps
    while (i > 0) {
      i -= 1
      b += DurationLite.timeAndDiscardResult(expr.run(ctx))
    }
    b.result()
  }

  def pool[F[_]](pool: ContextPool[F])
                (innerReps: Int,
                 expr: Expr[Any],
                 stopWhen: State => Boolean): PoolResult = {

    val warmupStart  = DurationLite.start()
    val innerExpr    = mkInnerExpr(innerReps, expr)
    val mutex        = new AnyRef
    var pending      = 0
    var stopped      = false
    var state        = State.empty
    var failure      = Option.empty[Throwable]
    val promiseFirst = Promise[State]()
    val promiseAll   = Promise[State]()

    def updateState(times: Vector[DurationLite]): Unit =
      state = State(
        outerReps = state.outerReps + 1,
        totalInnerReps = state.totalInnerReps + innerReps,
        repEvalTimes = times,
        totalWarmupTime = warmupStart.stop()
      )

    var task = Expr.unit

    def schedule(): Unit = {
      mutex.synchronized(pending += 1)
      pool.eval(task)
      ()
    }

    task = Expr.lift { ctx =>
      val result = Try(innerExpr.run(ctx))

      mutex.synchronized {
        pending -= 1

        result match {
          case Success(times) =>
            updateState(times)
            val stop = stopWhen(state) // always run, even if stopped, for side-effects like logging
            if (!stopped) {
              if (stop) {
                promiseFirst.success(state)
                stopped = true
              } else
                schedule()
            }

          case Failure(t) =>
            if (failure.isEmpty)
              failure = Some(t)
            if (!stopped) {
              promiseFirst.failure(t)
              stopped = true
            }
        }

        if (pending == 0)
          failure match {
            case None    => promiseAll.success(state)
            case Some(t) => promiseAll.failure(t)
          }
      }
      ()
    }

    for (_ <- 1 to pool.poolSize)
      schedule()

    PoolResult(promiseFirst.future, promiseAll.future)
  }

  /** @param warm The [[State]] when the warmup condition is first met.
    * @param done The [[State]] after all threads have finished.
    */
  final case class PoolResult(warm: Future[State], done: Future[State])
}
