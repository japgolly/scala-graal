package japgolly.scalagraal

import scala.concurrent.Future

trait ContextAsync {

  def eval[A](expr: Expr[A],
              additionalMetricWriter: ContextMetrics.Writer = ContextMetrics.Writer.Noop
             ): Future[Expr.Result[A]]

  def evalWithStats[A](expr: Expr[A],
                       additionalMetricWriter: ContextMetrics.Writer = ContextMetrics.Writer.Noop
                      ): Future[(Expr.Result[A], ContextMetrics.Stats)]

}
