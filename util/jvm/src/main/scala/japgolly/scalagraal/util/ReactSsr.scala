package japgolly.scalagraal.util

import japgolly.scalagraal._
import GraalJs._

/** Instructions for basic React SSR on the GraalVM:
  *
  * 1. Bundle React JS into your application yourself.
  * 2. Use [[Expr.requireFileOnClasspath()]] to load React JS files.
  * 3. Provide the above [[Expr]]s to [[ReactSsr.Setup.apply()]].
  * 4. Run the resulting [[Expr]] of above to initialise your [[ContextF]] instance(s).
  * 5. Optionally call [[ReactSsr.setUrl()]] if your component expects to read it (eg. has a router).
  * 6. Call [[ReactSsr.renderToString()]] or [[ReactSsr.renderToStaticMarkup()]] to render a component.
  *
  * See ScalaGraal's tests for concrete usage examples.
  */
object ReactSsr {

  private val SetWindowLocationFnName = "ScalaGraalSWL"

  object Setup {

    def apply(loadReact: Expr[Any]*): Expr[Unit] =
      for {
        _ <- preReact
        _ <- Expr.stdlibCosequenceDiscard(loadReact)
        _ <- postReact
      } yield ()

    private val addSetWindowLocationFn = {
      val mkObject =
        List("href", "origin", "protocol", "hostname", "port", "pathname", "search", "hash")
          .map(f => s"$f:i.$f()")
          .mkString("{", ",", "}")
      Expr(s"function $SetWindowLocationFnName(i){window.location=i?$mkObject:{}}")
    }

    private val defaultUserAgent: String = {
      val jvm = for {
        name <- sys.props.get("java.vm.name")
        ver  <- sys.props.get("java.vm.version")
      } yield s"$name ($ver)"
      jvm.getOrElse("")
    }

    /** Preparation of the environment required before loading React JS. */
    val preReact: Expr[Unit] =
      Expr.unit

    /** Preparation of the environment required after loading React JS, but before attempting SSR. */
    val postReact: Expr[Unit] =
      Expr.runAll(
        Expr.apply1[String](u => s"window = {console: console, navigator: {userAgent:$u}}")(defaultUserAgent),
        addSetWindowLocationFn,
      )
  }

  // ===================================================================================================================

  val setWindowLocation: WindowLocation => Expr[Unit] = {
    implicit val e = ExprParam.RawValueFn[WindowLocation]
    Expr.fn1(SetWindowLocationFnName).compile(_.void)
  }

  /** Sets `window.location` to an object representing the given URL.
    *
    * This is helpful but a better practice is to call [[WindowLocation.parse()]] and handle the None case yourself.
    */
  def setUrl(url: String): Expr[Unit] =
    setWindowLocation(WindowLocation.parse(url).orNull)

  val setUserAgent: String => Expr[Unit] =
    Expr.apply1(v => s"window.navigator.userAgent=$v").compile(_.void)

  /** Render a React element to its initial HTML. React will return an HTML string. You can use this method to generate
    * HTML on the server and send the markup down on the initial request for faster page loads and to allow search
    * engines to crawl your pages for SEO purposes.
    *
    * If you call ReactDOM.hydrate() on a node that already has this server-rendered markup, React will preserve it and
    * only attach event handlers, allowing you to have a very performant first-load experience.
    *
    * @param expr A JS expression determining what to render.
    * @return An [[Expr]] that evaluates to a String of HTML.
    */
  def renderToString(expr: String): Expr[String] =
    Expr(s"ReactDOMServer.renderToString($expr)").asString

  /** Similar to renderToString, except this doesn’t create extra DOM attributes that React uses internally, such as
    * data-reactroot. This is useful if you want to use React as a simple static page generator, as stripping away the
    * extra attributes can save some bytes.
    *
    * If you plan to use React on the client to make the markup interactive, do not use this method. Instead, use
    * [[renderToString]] on the server and ReactDOM.hydrate() on the client.
    *
    * @param expr A JS expression determining what to render.
    * @return An [[Expr]] that evaluates to a String of HTML.
    */
  def renderToStaticMarkup(expr: String): Expr[String] =
    Expr(s"ReactDOMServer.renderToStaticMarkup($expr)").asString
}
