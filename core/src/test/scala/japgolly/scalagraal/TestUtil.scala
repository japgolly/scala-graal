package japgolly.scalagraal

import cats.Eq

object TestUtil
  extends js.GraalJs
    with japgolly.microlibs.testutil.TestUtil
    with cats.instances.AnyValInstances
    with cats.instances.EitherInstances
    with cats.instances.OptionInstances
    with cats.instances.SetInstances
    with cats.instances.StringInstances
    with cats.instances.TupleInstances {

  lazy val sync = GraalContext.newContextPerUse()
//  val sync = ContextSync.Builder.newContextPerUse().writeMetrics(ContextMetrics.Println).build()

  def assertEvalResult[A: Eq](actual: Expr.Result[A], expect: A): Unit =
    assertEq(actual.left.map(_.toString), Right(expect))

  val inCI = Option(System.getenv("CI")).map(_.trim).exists(_.nonEmpty)
}
