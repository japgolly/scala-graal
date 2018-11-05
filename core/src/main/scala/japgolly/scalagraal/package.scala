package japgolly

package object scalagraal {

  implicit def ExprInterpolation(sc: StringContext): Expr.Interpolation =
    new Expr.Interpolation(sc)

}
