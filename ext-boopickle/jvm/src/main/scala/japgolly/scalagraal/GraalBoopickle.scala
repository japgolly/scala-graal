package japgolly.scalagraal

import boopickle._
import japgolly.scalagraal.ExprParam.RawValue
import org.graalvm.polyglot.Source

object GraalBoopickle {

  private[scalagraal] val tmpBinding = Language.Binding("__scalagraal_boopickle")

  private[scalagraal] val tmpToPickled = {
    val mkInt8Array = "const i=new Int8Array(a.limit());const b=a.array();let j=i.length;while(j-->0)i[j]=b[j]"
    val pickled     = "_scalagraal_bookpickle_Pickled(i)"
    val input       = Language.JS.polyglotImport(tmpBinding)
    val expr        = s"(function(a){$mkInt8Array;return $pickled})($input)"
    Source.create("js", expr)
  }

  implicit def exprParamBoopickle[A](implicit lang: Language, pickler: Pickler[A]): ExprParam[A] = {
    assert(lang == Language.JS, s"Unsupported language: $lang. Only JavaScript is supported.")
    ExprParam.CtxValueFn { a =>
      val byteBufferJvm = PickleImpl.intoBytes(a)
      ctx => {
        tmpBinding.withValue(ctx, byteBufferJvm) {
          RawValue(ctx.eval(tmpToPickled))
        }
      }
    }
  }
}
