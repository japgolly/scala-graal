package japgolly.scalagraal

import boopickle.{Pickler, UnpickleImpl}
import java.nio.ByteBuffer
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.{Int8Array, TypedArrayBuffer}

final class Pickled[A](buf: ByteBuffer) {
  private var buf2 = buf
  private var a: A = _

  def value(implicit p: Pickler[A]): A =
    if (buf2 eq null)
      a
    else {
      a = UnpickleImpl[A].fromBytes(buf2)
      buf2 = null
      a
    }
}

object Pickled {
  @JSExportTopLevel("_scalagraal_bookpickle_Pickled")
  def fromGraal(i: Int8Array) =
    new Pickled(TypedArrayBuffer.wrap(i))
}