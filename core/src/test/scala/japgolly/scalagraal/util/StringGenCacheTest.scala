package japgolly.scalagraal.util

import cats.Id
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scala.collection.compat.immutable.ArraySeq
import scalaz.std.list._
import scalaz.std.string._
import utest._

object StringGenCacheTest extends TestSuite {
  import StringGenCache._

  private val genChar: Gen[Char] =
    Gen.chooseGen(
      Gen.chooseChar('\n', "\r^$\\"),
      Gen.chooseInt(32).map(_.toChar),
      Gen.ascii,
      Gen.ascii,
      Gen.ascii,
      Gen.unicode,
    )

  private val genString: Gen[String] =
    Gen.chooseGen(
      genChar.string(0 to 4),
      genChar.string(0 to 4),
      genChar.string(0 to 4),
      Gen.unicode.string(0 to 100),
    )

  private case class Test[A](original: A => String, cached: A => String, value: A)

  private def prop[A] = Prop.equal[Test[A]]("Cache(f)(a) = f(a)").apply(
    actual = t => t.cached(t.value),
    expect = t => t.original(t.value))

  private def test[A](genA: Gen[A], noise: Int = 0)
                     (fn: ArraySeq[String] => A => String)
                     (implicit cache: StringGenCache[A]): Unit = {

    val genNoise: Gen[String] =
      Gen.chooseGen(
        genChar.string(0 to 4),
        genString,
      )

    val genFn: Gen[A => String] =
      for {
        prefix <- genNoise
        suffix <- genNoise
        user   <- genNoise.arraySeq[String](noise)
      } yield fn(user).andThen(prefix + _ + suffix) // .andThen(x => {println(x);x})

    def genTestsForFn(f: A => String): Gen[Test[A]] = {
      val cachedFn = cache[Id](f)
      genA.map(a => Test(original = f, cached = cachedFn, value = a))
    }

    val genTests: Gen[List[Test[A]]] =
      genFn.map(genTestsForFn(_).samples().take(100).toList)

    genTests.mustSatisfy(prop[A].forallF[List])
  }

  private case class Username(value: String) {
    def handle = "@" + value
  }

  private case class ExampleProps(user: Option[Username], notificationCount: Int)

  override def tests = Tests {
    "prop" - {
      "zero"   - test(genString)(_ => _ => "")
      "one"    - test(genString)(_ => identity)
      "two"    - test(genString, 1)(n => s => s + n(0) + s)
      "bool"   - test(Gen.boolean)(_ => _.toString)
      "unit"   - test(Gen.unit)(_ => _.toString)
      "option" - test(Gen.string.option)(_ => _.toString)
      "either" - test(Gen.string either Gen.string)(_ => _.toString)
//      "int"    - test(Gen.chooseInt(100))(_ => n => s"1 real value: $n!")
//      sum3
    }
  }
}
