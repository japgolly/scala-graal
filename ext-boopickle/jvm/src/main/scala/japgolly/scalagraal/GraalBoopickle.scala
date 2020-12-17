package japgolly.scalagraal

import boopickle._
import japgolly.scalagraal.ExprParam.RawValue
import japgolly.scalagraal.js.LanguageJs
import org.graalvm.polyglot.Source
import scala.annotation.nowarn

object GraalBoopickle {

  private[scalagraal] val tmpBinding = LanguageJs.Binding("ScalaGraalBoopickle")

  private[scalagraal] val tmpToPickled = {
    val pickled     = "ScalaGraalBookpicklePickled(new Int8Array(new ArrayBuffer(b)))"
    val input       = LanguageJs.polyglotImport(tmpBinding)
    val expr        = s"(function(b){return $pickled})($input)"
    Source.create("js", expr)
  }

  @nowarn("cat=unused")
  implicit def exprParamBoopickle[A](implicit js: LanguageJs.type, pickler: Pickler[A]): ExprParam[A] =
    ExprParam.CtxValueFn { a =>
      val byteBufferJvm = PickleImpl.intoBytes(a)
      ctx => {
        tmpBinding.withValue(ctx, byteBufferJvm) {
          RawValue(ctx.eval(tmpToPickled))
        }
      }
    }
}
