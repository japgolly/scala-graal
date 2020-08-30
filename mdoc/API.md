# API

* [Expr object](#expr-object)
* [Expr instance](#expr-instance)
* [ExprError](#exprerror)
* [GraalContext](#graalcontext)
* [GraalContextPool](#graalcontextpool)
* [Warmup](#warmup)
* [StrFnCache](#strfncache)
* ReactSSR
* WindowLocation

<br>

# Expr object

An `Expr` is an expression (or a function, or a procedure), in some embedded language that can be evaluated by GraalVM.

There are many ways to create an `Expr` instance:

```scala mdoc:silent
import org.graalvm.polyglot.Value
import japgolly.scalagraal._
import japgolly.scalagraal.js._
import GraalJs._

// Pass in code directly
Expr("console.log('Hello!')")

// Return your own values without interacting with the embedded language
Expr.unit
Expr.pure(123)
Expr.point(System.currentTimeMillis())

// Declare functions that are expected to exist in the embedded language
// (Change the "2" here to any arity.)
var mathPow: (Int, Int) => Expr[Int] =
  Expr.fn2[Int, Int]("Math.pow").compile(_.asInt)
//         ^^^^^^^^ fn inputs              ^^^^^ fn output

// You can do the same for your own expressions that take arguments
mathPow = Expr.apply2[Int, Int]((a, b) => s"$b == 0 ? 1 : Math.pow($a, $b)").compile(_.asInt)

// To apply arguments to your function, just call it.
// This is equivalent to "Math.pow(2, 4)" in JS.
var pow_2_4: Expr[Int] =
  mathPow(2, 4)

// You don't need to prepare functions as a separate step; you can evaluate expressions directly.
// (Note: if you're going to call your function repeatedly, it'll run faster if you prepare it in
//        a separate step like above, because scala-graal will pre-compile it.)
pow_2_4 = Expr.apply2((a, b) => s"Math.pow($a, $b)", 2, 4).asInt

// There are helpers for collections too.

// We've got good old "traverse", except its called "distribute" here
// because we're not traversing an Expr, it's a Scala collection that is traversed.
val names = List[String]( /* ... */ )
val nameLengths: Expr[List[Int]] =
  Expr.distribute(names)(name =>
    Expr.apply1(n => s"$n.length", name).asInt)

// We've got good old "sequence", except its called "cosequence" here
// because we're on the other side of the coin and don't have a "traverse"
// for Expr. See how we flip the positions of List and Expr:
val exprs   = List[Expr[Int]]( /* ... */ )
val allExprs: Expr[List[Int]] = Expr.cosequence(exprs)

// You can also queue a bunch of stuff to run sequentially
val setup: Expr[Unit] =
  Expr.runAll(allExprs, nameLengths, pow_2_4)

// To load and evaluate files on the classpath, there's:
Expr.fileOnClasspath("hello.js"): Expr[Option[Value]]

// And if you're ok throwing an ExprError if the file isn't found:
Expr.requireFileOnClasspath("hello.js"): Expr[Value]

// Finally there's also
// * Expr.lazily[A](a: => A): Expr[A]
// * Expr.suspend[A](a: => Expr[A]): Expr[A]
// * Expr.tailrec[A, B](init: => A)(f: A => Expr[Either[A, B]]): Expr[B]
```


# Expr instance

Once you've got an `Expr` instance there are many things you can do with it.

* Transformation and composition

  ```scala
  def map    [B](f   : A => B      ): Expr[B]
  def flatMap[B](f   : A => Expr[B]): Expr[B]
  def >>     [B](next:      Expr[B]): Expr[B]
  def <<     [B](prev:      Expr[B]): Expr[A]
  def flatTap[B](f   : A => Expr[B]): Expr[A]
  ```

* Specify the expected return value type

  ```scala
  def as[T]                          : Expr[T]
  def as[T]       (t: TypeLiteral[T]): Expr[T]
  def asOption                       : Expr[Option[A]]
  def asPromise                      : Expr[Future[Value]]
  def asPromise[B](f: Value => B)    : Expr[Future[B]]
  def void                           : Expr[Unit]

  def asBoolean: Expr[Boolean]
  def asByte   : Expr[Byte   ]
  def asDouble : Expr[Double ]
  def asFloat  : Expr[Float  ]
  def asInt    : Expr[Int    ]
  def asLong   : Expr[Long   ]
  def asShort  : Expr[Short  ]
  def asString : Expr[String ]

  def asBooleanArray: Expr[Array[Boolean]]
  def asByteArray   : Expr[Array[Byte   ]]
  def asDoubleArray : Expr[Array[Double ]]
  def asFloatArray  : Expr[Array[Float  ]]
  def asIntArray    : Expr[Array[Int    ]]
  def asLongArray   : Expr[Array[Long   ]]
  def asShortArray  : Expr[Array[Short  ]]
  def asStringArray : Expr[Array[String ]]

  def asBooleanList: Expr[List[Boolean]]
  def asByteList   : Expr[List[Byte   ]]
  def asDoubleList : Expr[List[Double ]]
  def asFloatList  : Expr[List[Float  ]]
  def asIntList    : Expr[List[Int    ]]
  def asLongList   : Expr[List[Long   ]]
  def asShortList  : Expr[List[Short  ]]
  def asStringList : Expr[List[String ]]

  def asBooleanVector: Expr[Vector[Boolean]]
  def asByteVector   : Expr[Vector[Byte   ]]
  def asDoubleVector : Expr[Vector[Double ]]
  def asFloatVector  : Expr[Vector[Float  ]]
  def asIntVector    : Expr[Vector[Int    ]]
  def asLongVector   : Expr[Vector[Long   ]]
  def asShortVector  : Expr[Vector[Short  ]]
  def asStringVector : Expr[Vector[String ]]
  ```

* Async utils (on `Expr[Future[A]]`)

  ```scala
  def await                         : Expr[A]
  def await       (atMost: Duration): Expr[A]
  def awaitAttempt                  : Expr[Either[Future[A], A]]
  def awaitAttempt(atMost: Duration): Expr[Either[Future[A], A]]
  ```

* Other utils

  ```scala

  // Assign the result to some variable
  def assignTo(name: String): Expr[A]

  // Create a new variable and assign the result to it
  def assignToNewVar(varName: String): Expr[A]

  // Measure how long the expression takes to evaluate
  // We return a DurationLite here which is a super-lightweight representation of Duration.
  // It's super-lightweight so that it's as fast as possible to construct, and uses a
  // minimal amount of memory possible. Expressions can sometimes be run in very hot
  // loops/envs, so we want to add as little overhead as possible if one wants to measure
  // and report, the timings of each execution.
  def timed: Expr[(A, DurationLite)]
  ```


# ExprError

There are a number of things that can go wrong when evaluating foreign language code:

* `ExprError`
  * `.InEval`
    * `AsyncFunctionFailed`
    * `ContextClosed`
    * `EvalError`
    * `UnsupportedLanguageOrMimeType`
  * `.InResult`
    * `ValueCastError` -- the value could not be converted to the expected type
    * `ValueError` -- a guest language error occurred during execution
    * `ValueIsNull` -- equivalent to `NullPointerException`
    * `ValueReprError` -- the value does not represent the expected type


# GraalContext

A `GraalContext` **provides** what GraalVM calls a "Context", which are stateful sandboxes/environments
in which you execute your foreign language code.

For synchronous usage:

* `GraalContext()` - defaults to `.fixedContext` below

* `GraalContext.fixedContext(options)` - creates a single context and uses it for all `.eval` calls.
  Consequences:

    * an `eval` call can modify global state and affect subsequent calls
    * it's fast because JIT will improve it the more you use it

* `GraalContext.newContextPerUse(options)` - creates a new, fresh context for each call to `.eval`
  Consequences:

    * `eval` calls are isolated and can't affect each other
    * it's slow because JIT only partially gets to optimise your code. JIT does more than nothing
      because under the hood, the contexts share a single "Engine" which some JIT. But the bulk of
      the JIT work is done within the context. Creating a new context every time means the JIT is
      always cold.

* `GraalContext.Builder.{fixedContext,newContextPerUse}(options).<config>.build()` -
  Create a builder that allows you to further configure your `GraalContext` before calling `.build()`.

  Configuration available:

    * `.useMutex(Boolean)` - whether or not eval calls need to be atomic, which avoids concurrency issues.
      <br/>Defaults to `true` for `fixedContext` and `false` for `newContextPerUse`.

    * Hooks to inject custom expressions at certain lifecycle events:

      * `onContextCreate(Expr[Any])`
      * `beforeEval(Expr[Any])`
      * `afterEval(Expr[Any])`
      * `onContextClose(Expr[Any])`

    * `writeMetrics(GraalContextMetrics.Writer)` to automatically collect metrics, and write them to the
      provided writer.

Once you've got an instance of `GraalContext`, you simply call `.eval(Expr[A])` to evaluate it.
The return type will be `Expr.Result[A]` which is `Either[ExprError, A]`.

> *Notes for advanced users:*
> * `Expr` is pure but `GraalContext` is not.
> * If you want to output `F[Expr.Result[A]]` like `IO[Expr.Result[A]]` or anything else,
>  there's a `.trans` method that accepts a natural transformation so that you can wrap
>  evaluations before they execute (as opposed to just wrapping the evaluation result).


# GraalContextPool

A `GraalContextPool[F]` is a thread pool of contexts.
Evaluation is asynchronous.
Out-of-the-box the `[F]` is `[Future]`, but once you've got an instance you can call `.trans`
to change `Future` to `IO` or any `F[_]` you like. Or you can specify the type you want in the
builder (see below).

The simplest way to create a `GraalContextPool` is by calling `GraalContextPool.fixedThreadPool(n)`
where `n` is the number of threads in the pool.

Just like `GraalContext`, there is a builder that allows you to customise how it works:

* `GraalContextPool.Builder.fixedThreadPool(n)`
  * Step 1: `.fixedContextPerThread` or `.newContextPerUse`
  * Step 2:
    * `.configure(f)` -- with `f` you can modify all the `GraalContext.Builder` settings
    * `.resultType[F]` -- `F` is `IO` or some other `F[_]` substitute for `Future`
    * `.awaitResultsWithTimeout` -- specify a global timeout for all calls to `.eval` on the instance
  * Finally call `.build`

Once you've got your `GraalContextPool`, usage is the same as a normal `GraalContext` except that it's
asynchronous so your results wil be wrapped in `Future` or whatever you specified.
Depending on your `F` type, results might not even have been evaluated but could be something that you evaluate in the future.


# Warmup

You can warmup your `GraalContext` or `GraalContextPool` by making it evaluation an expression over and over again
until a certain condition is met (usually a combination of max-time-elapsed and eval-speed-achieved).

```scala mdoc:compile-only
import japgolly.scalagraal.util._

val ctx = GraalContext()

// Warmup until the average last 10 evals completed in < 10ms, or give up after 30 sec.
Warmup.sync(ctx)(
  innerReps = 100,
  expr      = Expr("someFunctionOfMine()"),
  stopWhen  = s => s.lastEvalAverage(10).millis < 10 || s.totalWarmupTime.seconds > 30
)
```

And for a pool:

```scala mdoc:compile-only
import japgolly.scalagraal.util._

val ctxPool = GraalContextPool.Builder
  .fixedThreadPool(4)
  .fixedContextPerThread()
  .build()

// Warmup until the average last 10 evals completed in < 10ms, or give up after 30 sec.
Warmup.pool(ctxPool)(
  innerReps = 100,
  expr      = Expr("someFunctionOfMine()"),
  stopWhen  = s => s.lastEvalAverage(10).millis < 10 || s.totalWarmupTime.seconds > 30
)
```

I'm not able to recommend the best strategies here because I now use `CacheAndReplace` so that
I don't have to care about eval speed.
However, there is a [JMH benchmark around this](../benchmark/src/main/scala/japgolly/scalagraal/benchmark/WarmupBM.scala)
and I've included some results so
have a look at that to get a sense of warmup efficacy with regard to warmup parameters;
and keep in mind that the `Expr` being evaluated is hugely significant so best to test out
parameters with your own specific use case.


# StrFnCache

`StrFnCache` is a very powerful and very unconventional cache that is designed specifically for SSR.

It caches `A => F[String]` functions and makes them super-fast by executing once per path,
optimising and caching the result, and then using it as template for all subsequent calls.

"Paths" are independent from each other and are defined by `StrFnCacheParam` instances. There is typically
one path per sum type, for example `Option` has two paths: `None` and `Some[A]`; `String` only has one which is
itself.

Values (once in a path) must be completely opaque and not used to affect function logic or conditionality.
For example, a function that takes a name and prints it a few times is fine, but a function which branched
according to string length, or converted the string to uppercase would be broken by this cache because the caching
logic doesn't branch according to string length, and the caching logic doesn't convert strings to uppercase. You
can still have your super-fast cake and eat it too by creating your own customised `StrFnCacheParam` instance
that includes any value modification or branching you need. In such cases it would be wise to cross-compile the
logic rather than duplicate it, and/or modify your function (or React component) to accept inputs that have already
been processed.

Read more here: https://blog.shipreq.com/post/scala_react_and_ssr_part_2
