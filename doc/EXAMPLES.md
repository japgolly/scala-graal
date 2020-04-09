# Examples

* [Minimal example](#minimal-example)
* [Calling Scala.JS from JVM (using a binary protocol)](#calling-scalajs-from-jvm-using-a-binary-protocol)

<br>


Minimal example
===============

```scala
import japgolly.scalagraal._

// Use semantics and implicit config for JS
// (Graal also supports Python, R, Ruby, LLVM)
import GraalJs._

// 1. Pre-compile expression functions for fast invocation.
// 2. Typeclasses determine how to translate and/or marshall data from Scala to JS.
val expr: (Int, Int) => Expr[String] =
  Expr.apply2((a, b) => s"($a + $b) * 2 + '!'").compile(_.asString)

// Let's use a single synchronous JS environment
val ctx = ContextSync()

val result = ctx.eval(expr(3, 8))
assert(result == Right("22!"))
```


Calling Scala.JS from JVM (using a binary protocol)
===================================================

Shared code:

```scala
import boopickle.Default._

final case class ScalaData(a: Int, b: Int)
object ScalaData {
  // Binary format shared between JVM & JS
  implicit val pickler: Pickler[ScalaData] = generatePickler
}
```

Scala.JS code:

```scala
import scala.scalajs.js.annotation._

@JSExportTopLevel("myScalaJsFn") // This is how we'll call this fn from the JVM
def demo(p: Pickled[ScalaData]): String =
  s"a is ${p.value.a} and b is ${p.value.b}"
```

Scala (JVM) code:

```scala
import japgolly.scalagraal._
import GraalJs._
import GraalBoopickle._

val expr =
  for {
    _ <- Expr.requireFileOnClasspath("my_scalajs-fastopt.js") // Load our Scala.JS code
    d  = ScalaData(9999,3)                                    // Data to be converted JVM → binary → Scala.JS
    s <- Expr.fn1("myScalaJsFn", d).asString                  // Call Scala.JS with a case class
  } yield s

val result = ContextSync().eval(expr)
assert(result == Right("a is 9999 and b is 3"))
```
