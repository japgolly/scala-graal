package japgolly.scalagraal

import japgolly.scalagraal.TestData._
import scala.scalajs.js.annotation._

object ExampleGraalBoopickleClient {

  @JSExportTopLevel("example1")
  def example1(p: Pickled[Example]) =
    if (p eq null)
      "<< null?! >>"
    else
      s"<JS-${p.value.a}:${p.value.b}>"
}
