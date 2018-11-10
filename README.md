> I've begun work on #Scala / @graalvm integration. My goals:
>
> * FP & nice ergonomics from Scala
> * Performance & multithreading
> * React SSR
>
> https://twitter.com/japgolly/status/1058657919254679552


# Demo

```scala
// Use semantics and implicit config for JS
// (Graal also supports Python, R, Ruby, LLVM)
import GraalJs._

// 1. Pre-compile expression functions for fast invocation.
// 2. Typeclasses determine how to translate and/or marshall data from Scala to JS.
val expr: (Int, Int) => Expr[String] =
  Expr.compile2((a, b) => s"($a + $b) * 2 + '!'")(_.asString)

// Let's use a single synchronous JS evaluator/environment
val ctx = ContextSync()

val result = ctx.eval(expr(3, 8))
assert(result == Right("22!"))
```

# Demo: Call Scala.JS from JVM (binary protocol)

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
@JSExportTopLevel("myScalaJsFn") // This is how we'll call this fn from the JVM
def demo(p: Pickled[ScalaData]): String =
  s"a is ${p.value.a} and b is ${p.value.b}"
```

Scala (JVM) code:

```scala
// This will be converted from JVM -> binary -> Scala.JS
val data = ScalaData(9999,3)

val expr: Expr[Unit] =
  for {
    _ <- Expr.requireFileOnClasspath("my_scalajs-fastopt.js") // Load our Scala.JS code
    s <- Expr.apply1(a => s"myScalaJsFn($a)", data).asString  // Call Scala.JS with a case class
  } yield s

val result = ContextSync().eval(expr)
assert(result == Right("a is 9999 and b is 3"))
```


# Roadmap

* expressions
  * [x] composition
  * [x] purity
  * [x] parse results
  * [x] error handling
  * [x] null handling
  * [x] ~~binding interpolation~~ (REMOVED)
  * [x] binding typeclasses
  * [x] bindind codec (eg binary/json)
* service
  * [x] before/around/after hooks
  * [x] single
  * multi-threaded service
    * [x] fixed pool
    * [x] shutdown
  * [ ] synchronous resource pool
  * [ ] pure scheduling & execution (?)
  * [ ] higher-level profunctor-like
  * [ ] metrics
  * [ ] caching
* [ ] warmup
  * [ ] rules - eg. `up to 10000 reps/thread & up to 30 sec | until completes within 20ms`
  * [ ] on idle (?)
  * [ ] implement results of warmpup discussion with graal team
* React SSR
  * [ ] render util (so one needn't write direct JS)
  * [ ] `window` management
  * [ ] test util
* Integration
  * [ ] Scalaz
  * [ ] Cats
  * [ ] Cats Effect (?)
  * [ ] Prometheus
  * [x] BooPickle
  * [ ] circe
* build
  * [ ] travis
  * [ ] tut
