package japgolly.scalagraal

import scala.concurrent.Future

trait ContextAsync {
  def eval[A](expr: Expr[A]): Future[Expr.Result[A]]
}
