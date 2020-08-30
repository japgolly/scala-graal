package japgolly.scalagraal.util

import japgolly.microlibs.testutil.TestUtil._
import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import scalaz.std.list._
import scalaz.std.string._
import sourcecode.Line
import utest._

object StrFnCacheTest extends TestSuite {
  import StrFnCacheParam._

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

  private case class Test[A](original: A => String, cached: A => String, value: A) {
    override def toString = s"[$value] => [${original(value)}]"
  }

  private def prop[A] = Prop.equal[Test[A]]("Cache(f)(a) = f(a)").apply(
    actual = t => t.cached(t.value),
    expect = t => t.original(t.value))

  private def propTest[A](genA     : Gen[A],
                          noise    : Int = 0,
                          tailNoise: Boolean = true)
                         (fn       : Vector[String] => A => String)
                         (implicit cache: StrFnCacheParam[A]): Unit = {

    val genNoise: Gen[String] =
      Gen.chooseGen(
        genChar.string(0 to 4),
        genString,
      )

    val genTailNoise: Gen[String] =
      if (tailNoise) genNoise else Gen.pure("")

    val genFn: Gen[A => String] =
      for {
        prefix <- genTailNoise
        suffix <- genTailNoise
        user   <- genNoise.vector(noise)
      } yield fn(user).andThen(prefix + _ + suffix) // .andThen(x => {println(x);x})

    def genTestsForFn(f: A => String): Gen[Test[A]] = {
      val cachedFn = StrFnCache.id(f)
      genA.map(a => Test(original = f, cached = cachedFn, value = a))
    }

    val genTests: Gen[List[Test[A]]] =
      genFn.map(genTestsForFn(_).samples().take(100).toList)

    genTests.mustSatisfy(prop[A].forallF[List])
  }

  private def test[A](values: A*)(f: A => String)(implicit l: Line, cache: StrFnCacheParam[A]): Unit = {
    val c = StrFnCache.id(f)
    for (v <- values) {
      val e = f(v)
      val a = c(v)
      assertEq(s"value: $v", actual = a, expect = e)
    }
  }

  private case class Username(value: String) {
    def handle = "@" + value
  }

  private case class ExampleProps(user: Option[Username], notificationCount: Int) {
    def render = user match {
      case Some(u) => s"Hello again ${u.handle}, you have ${notificationCount} notifications."
      case None    => s"Hi there! You have ${notificationCount} notifications."
    }
  }

  private val genExampleProps = Gen.lift2(genString.map(Username).option, Gen.int)(ExampleProps)

  private sealed trait SumTest {
    def render = this match {
      case SumTest.S1 => "wow"
      case SumTest.S2 => "oho"
      case SumTest.S3 => "sik"
    }
  }

  private object SumTest {
    case object S1 extends SumTest
    case object S2 extends SumTest
    case object S3 extends SumTest
    val gen = Gen.choose[SumTest](S1, S2, S3)
    implicit val cache: StrFnCacheParam[SumTest] = StrFnCacheParam.divide3(
      StrFnCacheParam.const[S1.type](S1),
      StrFnCacheParam.const[S2.type](S2),
      StrFnCacheParam.const[S3.type](S3),
    )
  }

  private implicit val cacheShort        = StrFnCacheParam.short(3)
  private implicit val cacheInt          = StrFnCacheParam.int(3)
  private implicit val cacheLong         = StrFnCacheParam.long(3)
  private implicit val cacheUsername     = StrFnCacheParam.apply1(Username.apply)(_.value)
  private implicit val cacheExampleProps = StrFnCacheParam.apply2(ExampleProps.apply)(p => (p.user, p.notificationCount))

  override def tests = Tests {

    "manual" - {
      "short" - test[Short](9, 0, 1, 3, -3, 33, -33)(n => s"$n 0 1 -3 $n $n-$n 33 -33 $n")
      "int"   - test[Int  ](9, 0, 1, 3, -3, 33, -33)(n => s"$n 0 1 -3 $n $n-$n 33 -33 $n")
      "long"  - test[Long ](9, 0, 1, 3, -3, 33, -33)(n => s"$n 0 1 -3 $n $n-$n 33 -33 $n")
    }

    "prop" - {
      "zero"       - propTest(genString)(_ => _ => "")
      "one"        - propTest(genString)(_ => identity)
      "two"        - propTest(genString, 1)(n => s => s + n(0) + s)
      "bool"       - propTest(Gen.boolean)(_ => _.toString)
      "unit"       - propTest(Gen.unit)(_ => _.toString)
      "option"     - propTest(Gen.string.option)(_ => _.toString)
      "either"     - propTest(Gen.string either Gen.string)(_ => _.toString)
      "int"        - propTest(Gen.int, tailNoise = false)(_ => n => s"[ 0 1 2 ${Int.MaxValue} ${Int.MinValue} ] real value: $n!")
      "long"       - propTest(Gen.long, tailNoise = false)(_ => n => s"[ 0 1 2 ${Long.MaxValue} ${Long.MinValue} ] real value: $n!")
      "stringPair" - propTest(genString *** genString)(_ => x => s"${x._1} / ${x._2}")
      "egProps"    - propTest(genExampleProps, tailNoise = false)(_ => _.render)
      "divide3"    - propTest(SumTest.gen, tailNoise = false)(_ => _.render)
    }
  }
}
