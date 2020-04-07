package japgolly.scalagraal

import org.graalvm.polyglot.{PolyglotException, Value}

sealed abstract class ExprError(t: Throwable) extends RuntimeException(t.getMessage, t) {
  val underlying: Throwable
}

object ExprError {

  // ===================================================================================================================

  sealed abstract class InEval(t: Throwable) extends ExprError(t)

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

  final case class EvalError(underlying: PolyglotException) extends InEval(underlying)

  final case class ContextClosed(underlying: IllegalStateException) extends InEval(underlying)

  final case class UnsupportedLanguageOrMimeType(underlying: IllegalArgumentException) extends InEval(underlying)

  final case class AsyncFunctionFailed(failure: Value) extends InEval(new RuntimeException(s"AsyncFunctionFailed: $failure")) {
    override val underlying = new RuntimeException(s"AsyncFunctionFailed: $failure")
  }

  // ===================================================================================================================

  sealed abstract class InResult(t: Throwable) extends ExprError(t) {
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
  final case class ValueError(value: Value, underlying: PolyglotException) extends InResult(underlying)

  final case class ValueIsNull(value: Value, underlying: NullPointerException) extends InResult(underlying)

  /** the value could not be converted to the expected type */
  final case class ValueCastError(value: Value, underlying: ClassCastException) extends InResult(underlying)

  /** the value does not represent the expected type */
  final case class ValueReprError(value: Value, underlying: UnsupportedOperationException) extends InResult(underlying)
}
