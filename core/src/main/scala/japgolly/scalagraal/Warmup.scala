package japgolly.scalagraal

import scala.annotation.tailrec

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

  def apply(ctx: ContextSync)
           (innerReps: Int,
            expr: Expr[Any],
            stopWhen: State => Boolean): State = {

    val warmupStart = DurationLite.start()

    val innerExpr: Expr[Vector[DurationLite]] = Expr.lift { ctx =>
      val b = Vector.newBuilder[DurationLite]
      b.sizeHint(innerReps)
      var i = innerReps
      while (i > 0) {
        i -= 1
        b += DurationLite.timeAndDiscardResult(expr.run(ctx))
      }
      b.result()
    }


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

    go(State(0, 0, Vector.empty, DurationLite.Zero))
  }

  // TODO what about ContextAsync?
}
