package japgolly.scalagraal

import org.graalvm.polyglot.Context

// In Dotty this can be made contravariant
sealed trait ExprParam[A]

object ExprParam {
  final case class Const   [A](source: String)               extends ExprParam[A]
  final case class Literal [A](mkSource: A => String)        extends ExprParam[A]
  final case class Polyglot[A](mkValue: A => Any)            extends ExprParam[A]
  final case class Custom  [A](mkValue: A => Context => Any) extends ExprParam[A]
}

