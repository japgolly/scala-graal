package japgolly

import org.graalvm.polyglot.Value

package object scalagraal {

  @inline implicit def scalagraalValueExt(a: Value): Extensions.ValueExt =
    new Extensions.ValueExt(a)
}
