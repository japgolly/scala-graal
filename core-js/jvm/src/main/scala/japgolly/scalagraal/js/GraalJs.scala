package japgolly.scalagraal.js

import japgolly.scalagraal._

object GraalJsComponents {
  import ExprParam._

  trait JsValues {
    implicit val exprParamJsUnit: ExprParam[Unit] = SourceConst("undefined")
  }

  trait Stdlib {
    implicit def exprParamOptionF[A](implicit p: SourceFn[A]): SourceFn[Option[A]] =
      SourceFn {
        case Some(a) => p.mkSource(a)
        case None    => "null"
      }

    implicit def exprParamOptionV[A](implicit p: ValueFn[A]): ValueFn[Option[A]] =
      ValueFn {
        case Some(a) => p.mkValue(a)
        case None    => RawValue.Null
      }

    implicit def exprParamOptionC[A](implicit p: CtxValueFn[A]): CtxValueFn[Option[A]] =
      CtxValueFn {
        case Some(a) => p.mkValue(a)
        case None    => _ => RawValue.Null
      }
  }
}

trait GraalJs
  extends ExprParam.Defaults
     with GraalJsComponents.JsValues
     with GraalJsComponents.Stdlib {

  implicit final def graalLanguage: LanguageJs.type = LanguageJs
}

object GraalJs extends GraalJs
