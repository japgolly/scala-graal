package japgolly.scalagraal

trait ContextF[F[_]] {

  final def eval[A](expr: Expr[A]): F[Expr.Result[A]] =
    eval(expr, ContextMetrics.Writer.Noop)

  final def evalWithStats[A](expr: Expr[A]): F[ContextMetrics.AndExprResult[A]] =
    evalWithStats(expr, ContextMetrics.Writer.Noop)

  // ===================================================================================================================

  /**
    * @param metricWriter This is in addition to any other metrics writers supplied in the construction of the context.
    */
  def eval[A](expr: Expr[A], metricWriter: ContextMetrics.Writer): F[Expr.Result[A]]

  /**
    * @param metricWriter This is in addition to any other metrics writers supplied in the construction of the context.
    */
  def evalWithStats[A](expr: Expr[A], metricWriter: ContextMetrics.Writer): F[ContextMetrics.AndExprResult[A]]

}
