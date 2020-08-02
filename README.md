# scala-graal

[![Build Status](https://travis-ci.org/japgolly/scala-graal.svg?branch=master)](https://travis-ci.org/japgolly/scala-graal)

```scala
libraryDependencies ++= Seq(
  "com.github.japgolly.scala-graal"  %% "core"           % "0.5.0"
  "com.github.japgolly.scala-graal" %%% "core-js"        % "0.5.0"
  "com.github.japgolly.scala-graal" %%% "ext-boopickle"  % "0.5.0"
  "com.github.japgolly.scala-graal"  %% "ext-prometheus" % "0.5.0"
)
```


# Goals

* Make it quick, easy and safe to interface with embedded languages from Scala
* Hide and automate a lot of required [GraalVM](https://www.graalvm.org) boilerplate
* Support [React SSR](https://css-tricks.com/server-side-react-rendering/) for [Scala.JS](https://www.scala-js.org/) applications


# Demo

```scala
import japgolly.scalagraal._

// Use semantics and implicit config for JS
// (GraalVM also supports Python, R, Ruby, LLVM)
import japgolly.scalagraal.js._
import GraalJs._

// 1. Pre-compile expression functions for fast invocation.
// 2. Typeclasses translate and/or marshall data from JVM to JS.
val expr: (Int, Int) => Expr[String] =
  Expr.apply2((a, b) => s"($a + $b) * 2 + '!'").compile(_.asString)

// Use a basic synchronous JS environment
val ctx = GraalContext()

val result = ctx.eval(expr(3, 8))
assert(result == Right("22!"))
```


# Features

* Expressions
  * composition
  * purity
  * result parsing
  * error handling
  * null handling
  * binding typeclasses
  * binding codecs (eg binary/json)
* Service ("Context")
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
* Integrations
  * [Prometheus](https://prometheus.io) - export metrics to Prometheus
  * [BooPickle](https://github.com/suzaku-io/boopickle) - marshall data back and forth using binary codecs


# Learning

* [API walkthrough](doc/API.md)
* [Recipes](doc/RECIPES.md)

