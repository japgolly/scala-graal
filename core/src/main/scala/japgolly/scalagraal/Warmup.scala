package japgolly.scalagraal

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

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
           stopWhen: State => Boolean): State = {

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

  def pool(pool: ContextPool)
          (innerReps: Int,
           expr: Expr[Any],
           stopWhen: State => Boolean): PoolResult = {

    val warmupStart  = DurationLite.start()
    val innerExpr    = mkInnerExpr(innerReps, expr)
    val mutex        = new AnyRef
    var futures      = List.fill(pool.poolSize)(pool.eval(innerExpr))
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

    def onFailure(t: Throwable) = {
      if (failure.isEmpty)
        failure = Some(t)
      if (!stopped) {
        promiseFirst.failure(t)
        stopped = true
      }
    }

    def onComplete(f: Future[Expr.Result[Vector[DurationLite]]]): Unit =
      f.onComplete { result =>
        mutex.synchronized {
          futures = futures.filter(_ ne f)

          result match {
            case Success(Right(times)) =>
              updateState(times)
              val stop = stopWhen(state) // always run, even if stopped, for side-effects like logging
              if (!stopped) {
                if (stop) {
                  promiseFirst.success(state)
                  stopped = true
                } else {
                  val f2 = pool.eval(innerExpr)
                  futures ::= f2
                  onComplete(f2)
                }
              }

            case Success(Left(e)) => onFailure(e)
            case Failure(e)       => onFailure(e)
          }

          if (futures.isEmpty)
            failure match {
              case None    => promiseAll.success(state)
              case Some(t) => promiseAll.failure(t)
            }
        } // mutex
      }

    futures.foreach(onComplete)

    PoolResult(promiseFirst.future, promiseAll.future)
  }

  // TODO rename fields
  final case class PoolResult(first: Future[State], all: Future[State])
}
