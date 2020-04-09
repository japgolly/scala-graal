# scala-graal

[![Build Status](https://travis-ci.org/japgolly/scala-graal.svg?branch=master)](https://travis-ci.org/japgolly/scala-graal)

```scala
libraryDependencies ++= Seq(
  "com.github.japgolly.scala-graal" % "core" % "@VERSION@"
)
```

# Demo

```scala mdoc:silent
import japgolly.scalagraal._

// Use semantics and implicit config for JS
// (Graal also supports Python, R, Ruby, LLVM)
import GraalJs._

// 1. Pre-compile expression functions for fast invocation.
// 2. Typeclasses determine how to translate and/or marshall data from Scala to JS.
val expr: (Int, Int) => Expr[String] =
  Expr.apply2((a, b) => s"($a + $b) * 2 + '!'").compile(_.asString)

// Let's use a single synchronous JS evaluator/environment
val ctx = ContextSync()

val result = ctx.eval(expr(3, 8))
assert(result == Right("22!"))
```
