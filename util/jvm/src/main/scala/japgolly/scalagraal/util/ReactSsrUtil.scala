package japgolly.scalagraal.util

import japgolly.scalagraal._
import GraalJs._

object ReactSsrUtil {

  private val SetWindowLocationFnName = "ScalaGraalSWL"

  object Setup {

    private val addSetWindowLocationFn = {
      val mkObject =
        List("href", "origin", "protocol", "hostname", "port", "pathname", "search", "hash")
          .map(f => s"$f:i.$f()")
          .mkString("{", ",", "}")
      Expr(s"function $SetWindowLocationFnName(i){window.location=i?$mkObject:{}}")
    }

    val postReact = Expr.runAll(
      Expr("window = {console: console, navigator: {userAgent:''}}"),
      addSetWindowLocationFn,
    )

    def apply(loadReact: Expr[Any]*): Expr[Unit] =
      for {
        _ <- Expr.stdlibCosequenceDiscard(loadReact)
        _ <- postReact
      } yield ()
  }

  val setWindowLocation: WindowLocation => Expr[Unit] = {
    implicit val e = ExprParam.RawValueFn[WindowLocation]
    Expr.compileFnCall1(SetWindowLocationFnName)(_.void)
  }

  /** Sets `window.location` to an object representing the given URL.
    *
    * This is helpful but a better practice is to call [[WindowLocation.parse()]] and handle the None case yourself.
    */
  def setUrl(url: String): Expr[Unit] =
    setWindowLocation(WindowLocation.parse(url).orNull)

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
