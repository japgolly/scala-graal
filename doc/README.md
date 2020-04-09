# scala-graal

[![Build Status](https://travis-ci.org/japgolly/scala-graal.svg?branch=master)](https://travis-ci.org/japgolly/scala-graal)

```scala
libraryDependencies ++= Seq(
  "com.github.japgolly.scala-graal" % "core" % "0.4.0"
)
```


# Demo

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


# Features

* Expressions
  * composition
  * purity
  * parse results
  * error handling
  * null handling
  * binding typeclasses
  * bindind codec (eg binary/json)
* Service
  * single-threaded
  * multi-threaded pool
  * synchronous
  * asynchronous
  * eval with optional time limit
  * before/around/after hooks
  * automatic metrics
* Warmup
  * ability to warmup VM
  * rules (eg. `up to 10000 reps/thread & up to 30 sec | until completes within 20ms`)
* React SSR (Server-Side Rendering)
  * support for rendering JS components from JVM
  * conversion to constant-time (conditionally)
  * `window` and especially `window.location` management
* Integration
  * Prometheus - export metrics to Prometheus
  * BooPickle - marshall data back and forth using binary codecs
