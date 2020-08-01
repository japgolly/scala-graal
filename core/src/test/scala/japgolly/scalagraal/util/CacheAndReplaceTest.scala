package japgolly.scalagraal.util

import java.util.UUID
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scalaz.std.string._
import utest._

object CacheAndReplaceTest extends TestSuite {

  private val genChar: Gen[Char] =
    Gen.chooseGen(
      Gen.chooseChar('\n', "\r^$\\"),
      Gen.chooseInt(32).map(_.toChar),
      Gen.ascii,
      Gen.ascii,
      Gen.unicode,
    )

  override def tests = Tests {
    "prop" - {
      type T = (String, String) => String
      type D = (String, String, T)

      val id1 = s"[${UUID.randomUUID()}${UUID.randomUUID()}]"
      val id2 = s"[${UUID.randomUUID()}${UUID.randomUUID()}]"

      val gen: Gen[D] =
        for {
          s <- Gen.chooseInt(20)
          r <- genChar.string(0 to 4).list(8)
          f <- Gen.choose(id1, id2 :: r: _*).list(s)
          a <- genChar.string(0 to 4)
          b <- genChar.string(0 to 4)
        } yield {
          val run: T = (a, b) =>
            f.iterator.map {
              case `id1` => a
              case `id2` => b
              case x     => x
            }.mkString
          (a, b, run)
        }

      val prop = Prop.equal[D]("T(f)(a,b) = f(a,b)")(
        { case (a, b, t) => CacheAndReplace.compile2(t).apply(a, b) },
        { case (a, b, t) => t(a, b) })

      gen.mustSatisfy(prop)
//      gen.bugHunt(seedStart = 10000)(prop)
    }
  }
}
