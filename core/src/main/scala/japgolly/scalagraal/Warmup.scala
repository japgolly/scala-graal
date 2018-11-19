package japgolly.scalagraal

import scala.annotation.tailrec

object Warmup {

  final case class State(outerReps: Int,
                         totalInnerReps: Int,
                         firstEvalInLastRep: DurationLite,
                         lastEvalInLastRep: DurationLite,
                         totalWarmupTime: DurationLite) {
    override def toString =
      s"Warmup.State(outerReps = $outerReps, totalInnerReps = $totalInnerReps, firstEvalInLastRep = $firstEvalInLastRep, lastEvalInLastRep = $lastEvalInLastRep, totalWarmupTime = ${totalWarmupTime.toStrSec})"
  }

  def apply(ctx: ContextSync)
           (innerReps: Int,
            expr: Expr[Any],
            stopWhen: State => Boolean): State = {

    val warmupStart = DurationLite.start()

    val innerExpr: Expr[(DurationLite, DurationLite)] = Expr.lift { ctx =>
      val first = DurationLite.timeAndDiscardResult(expr.run(ctx))
      var i = innerReps - 2
      while (i > 0) {
        i -= 1
        expr.run(ctx)
      }
      val last =
        if (innerReps > 1)
          DurationLite.timeAndDiscardResult(expr.run(ctx))
        else
          first
      (first, last)
    }


    @tailrec def go(s: State): State =
      if (s.outerReps != 0 && stopWhen(s))
        s
      else {
        val result = ctx.eval(innerExpr).fold(throw _, identity)
        go(State(
          outerReps = s.outerReps + 1,
          totalInnerReps = s.totalInnerReps + innerReps,
          firstEvalInLastRep = result._1,
          lastEvalInLastRep = result._2,
          totalWarmupTime = warmupStart.stop()
        ))
      }

    go(State(0, 0, DurationLite.Zero, DurationLite.Zero, DurationLite.Zero))
  }

  // TODO what about ContextAsync?
}
