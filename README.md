> I've begun work on #Scala / @graalvm integration. My goals:
>
> * FP & nice ergonomics from Scala
> * Performance & multithreading
> * React SSR
>
> https://twitter.com/japgolly/status/1058657919254679552

--

## ROADMAP

* expressions
  * [x] composition
  * [x] purity
  * [x] parse results
  * [x] error handling
  * [x] null handling
  * [ ] bindings
* service
  * [x] before/around/after hooks
  * [x] single
* multi-threaded service
  * [x] fixed pool
  * [ ] warmup
  * [ ] shutdown
  * [ ] metrics
  * [ ] caching
* React SSR
  * [ ] render util (so one needn't write direct JS)
  * [ ] `window` management
