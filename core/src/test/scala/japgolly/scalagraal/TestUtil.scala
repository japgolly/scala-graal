package japgolly.scalagraal

import scalaz.Equal

object TestUtil
  extends GraalJs
    with japgolly.microlibs.testutil.TestUtil
    with scalaz.std.AnyValInstances
    with scalaz.std.EitherInstances
    with scalaz.std.OptionInstances
    with scalaz.std.SetInstances
    with scalaz.std.StringInstances
    with scalaz.std.TupleInstances {

  val sync = ContextSync2.newContextPerUse()
//  val sync = ContextSync2.Builder.newContextPerUse().writeMetrics(ContextMetrics.Println).build()

  def assertEvalResult[A: Equal](actual: Expr.Result[A], expect: A): Unit =
    assertEq(actual.left.map(_.toString), Right(expect))

}
