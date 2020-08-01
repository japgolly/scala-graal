package japgolly

import org.graalvm.polyglot.Value

package object scalagraal {

  @inline implicit def scalaGraalValueExt(a: Value): ScalaGraalValueExt =
    new ScalaGraalValueExt(a)
}
