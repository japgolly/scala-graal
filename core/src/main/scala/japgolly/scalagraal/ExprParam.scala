package japgolly.scalagraal

import org.graalvm.polyglot.Context

// In Dotty this can be made contravariant
sealed trait ExprParam[A]

object ExprParam {

  final case class SourceConst[A](source: String)               extends ExprParam[A]
  final case class SourceFn   [A](mkSource: A => String)        extends ExprParam[A]
  final case class ValueFn    [A](mkValue: A => Any)            extends ExprParam[A]
  final case class CtxValueFn [A](mkValue: A => Context => Any) extends ExprParam[A]

  trait Primitives {
    implicit val exprParamBoolean: ExprParam[Boolean] = ValueFn(a => a)
    implicit val exprParamByte   : ExprParam[Byte   ] = ValueFn(a => a)
    implicit val exprParamShort  : ExprParam[Short  ] = ValueFn(a => a)
    implicit val exprParamInt    : ExprParam[Int    ] = ValueFn(a => a)
    implicit val exprParamLong   : ExprParam[Long   ] = ValueFn(a => a)
    implicit val exprParamFloat  : ExprParam[Float  ] = ValueFn(a => a)
    implicit val exprParamDouble : ExprParam[Double ] = ValueFn(a => a)
    implicit val exprParamString : ExprParam[String ] = ValueFn(a => a)
  }

  trait JsValues {
    implicit val exprParamJsUnit: ExprParam[Unit] = SourceConst("undefined")
  }

}

