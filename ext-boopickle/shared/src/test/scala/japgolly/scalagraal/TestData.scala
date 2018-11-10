package japgolly.scalagraal

import boopickle.Default._

object TestData {

  final case class Example(a: Int, b: Int)

  object Example {
    implicit val pickler: Pickler[Example] = generatePickler
  }

}
