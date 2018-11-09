package japgolly.scalagraal

import japgolly.scalagraal.ExprParam.SourceConst

object GraalJsComponents {

  trait JsValues {
    implicit val exprParamJsUnit: ExprParam[Unit] = SourceConst("undefined")
  }

}

trait GraalJs
  extends ExprParam.Defaults
     with GraalJsComponents.JsValues {

  implicit final val graalLanguage: Language = Language.JS
}

object GraalJs extends GraalJs