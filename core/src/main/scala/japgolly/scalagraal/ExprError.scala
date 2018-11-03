package japgolly.scalagraal

import org.graalvm.polyglot.{PolyglotException, Value}

sealed trait ExprError extends RuntimeException {
  val underlying: Throwable
}

object ExprError {

  // ===================================================================================================================

  sealed trait InEval extends ExprError

  object InEval {
    def capture[A](a: => A): A =
      try a catch { case t: Throwable => throw unapply(t).getOrElse(t) }

    def unapply(t: Throwable): Option[InEval] = t match {
      case t: InEval                        => Some(t)
      case t: IllegalArgumentException      => Some(ExprError.UnsupportedLanguageOrMimeType(t))
      case t: IllegalStateException         => Some(ExprError.ContextClosed(t))
      case t: PolyglotException             => Some(ExprError.EvalError(t))
    }
  }

  final case class EvalError(underlying: PolyglotException) extends InEval

  final case class ContextClosed(underlying: IllegalStateException) extends InEval

  final case class UnsupportedLanguageOrMimeType(underlying: IllegalArgumentException) extends InEval

  // ===================================================================================================================

  sealed trait InResult extends ExprError {
    val value: Value
  }

  object InResult {
    def capture[A](v: Value, f: Value => A): A =
      try f(v) catch { case t: Throwable => throw unapply(t).fold(t)(_(v)) }

    def unapply(t: Throwable): Option[Value => InResult] = t match {
      case t: InResult                      => Some(_ => t)
      case t: PolyglotException             => Some(ExprError.ValueError(_, t))
      case t: NullPointerException          => Some(ExprError.ValueIsNull(_, t))
      case t: ClassCastException            => Some(ExprError.ValueCastError(_, t))
      case t: UnsupportedOperationException => Some(ExprError.ValueReprError(_, t))
    }
  }

  /** a guest language error occurred during execution */
  final case class ValueError(value: Value, underlying: PolyglotException) extends InResult

  final case class ValueIsNull(value: Value, underlying: NullPointerException) extends InResult

  /** the value could not be converted to the expected type */
  final case class ValueCastError(value: Value, underlying: ClassCastException) extends InResult

  /** the value does not represent the expected type */
  final case class ValueReprError(value: Value, underlying: UnsupportedOperationException) extends InResult
}
