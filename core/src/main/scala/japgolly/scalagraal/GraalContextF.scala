package japgolly.scalagraal

trait GraalContextF[F[_]] { self =>

  final def eval[A](expr: Expr[A]): F[Expr.Result[A]] =
    eval(expr, GraalContextMetrics.Writer.Noop)

  final def evalWithStats[A](expr: Expr[A]): F[GraalContextMetrics.AndExprResult[A]] =
    evalWithStats(expr, GraalContextMetrics.Writer.Noop)

  // ===================================================================================================================

  /**
    * @param metricWriter This is in addition to any other metrics writers supplied in the construction of the context.
    */
  def eval[A](expr: Expr[A], metricWriter: GraalContextMetrics.Writer): F[Expr.Result[A]]

  /**
    * @param metricWriter This is in addition to any other metrics writers supplied in the construction of the context.
    */
  def evalWithStats[A](expr: Expr[A], metricWriter: GraalContextMetrics.Writer): F[GraalContextMetrics.AndExprResult[A]]

  // ===================================================================================================================

  def trans[G[_]](f: ScalaGraalEffect.Trans[F, G]): GraalContextF[G] =
    new GraalContextF[G] {
      override def eval[A](expr: Expr[A], metricWriter: GraalContextMetrics.Writer): G[Expr.Result[A]] =
        f(self.eval(expr, metricWriter))

      override def evalWithStats[A](expr: Expr[A], metricWriter: GraalContextMetrics.Writer): G[GraalContextMetrics.AndExprResult[A]] =
        f(self.evalWithStats(expr, metricWriter))
    }
}
