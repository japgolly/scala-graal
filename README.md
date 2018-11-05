> I've begun work on #Scala / @graalvm integration. My goals:
>
> * FP & nice ergonomics from Scala
> * Performance & multithreading
> * React SSR
>
> https://twitter.com/japgolly/status/1058657919254679552


# Demo

```scala
implicit val lang = Language.JS
val ctx = ContextSync()

val (a,b) = (3,8)
val expr = js"($a + $b) * 2".asInt

val Right(result) = ctx(expr)
assert(result == 22)
```

# Roadmap

* expressions
  * [x] composition
  * [x] purity
  * [x] parse results
  * [x] error handling
  * [x] null handling
  * [x] bindings (via interpolation)
* service
  * [x] before/around/after hooks
  * [x] single
  * [ ] pure scheduling & execution (?)
* multi-threaded service
  * [x] fixed pool
  * [ ] warmup
    * [ ] rules - eg. `up to 10000 reps/thread & up to 30 sec | until completes within 20ms`
    * [ ] on idle (?)
    * [ ] implement results of warmpup discussion with graal team
  * [x] shutdown
  * [ ] metrics
  * [ ] caching
* React SSR
  * [ ] render util (so one needn't write direct JS)
  * [ ] `window` management
  * [ ] test util
* Integration
  * [ ] Scalaz
  * [ ] Cats
  * [ ] Cats Effect (?)
  * [ ] Prometheus
