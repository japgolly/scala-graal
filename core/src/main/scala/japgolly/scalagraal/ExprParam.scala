package japgolly.scalagraal

import org.graalvm.polyglot.Context

// In Dotty this can be made contravariant
sealed trait ExprParam[A]

object ExprParam {

  final case class SourceConst[A](source: String)               extends ExprParam[A]
  final case class SourceFn   [A](mkSource: A => String)        extends ExprParam[A]
  final case class ValueFn    [A](mkValue: A => Any)            extends ExprParam[A]
  final case class CtxValueFn [A](mkValue: A => Context => Any) extends ExprParam[A]
}

