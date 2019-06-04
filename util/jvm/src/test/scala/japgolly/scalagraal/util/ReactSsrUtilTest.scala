package japgolly.scalagraal.util

import japgolly.scalagraal._
import TestUtil._
import utest._

object ReactSsrUtilTest extends TestSuite {

  private lazy val setup =
    ReactSsrUtil.Setup(
      Expr.requireFileOnClasspath("react.production.min.js"),
      Expr.requireFileOnClasspath("react-dom-server.browser.production.min.js"),
    )

  private def render(a: String): Expr[String] =
    Expr(s"ReactDOMServer.renderToStaticMarkup(React.createElement('div', null, $a))").asString

  override def tests = Tests {
    'hehe - {
      val expr = setup >> render("'hehe'")
      val r = sync.eval(expr)
      assertEvalResult(r, "<div>hehe</div>")
    }

    'setUrl - {
      val test = for {
        _        <- setup
        _        <- ReactSsrUtil.setUrl("http://blah.com:123/hehe?q#h")
        href     <- Expr("window.location.href    ").asString
        origin   <- Expr("window.location.origin  ").asString
        protocol <- Expr("window.location.protocol").asString
        hostname <- Expr("window.location.hostname").asString
        port     <- Expr("window.location.port    ").asString
        pathname <- Expr("window.location.pathname").asString
        search   <- Expr("window.location.search  ").asString
        hash     <- Expr("window.location.hash    ").asString
      } yield {
        assertEq(href    , "http://blah.com:123/hehe?q#h")
        assertEq(origin  , "http://blah.com:123")
        assertEq(protocol, "http:")
        assertEq(hostname, "blah.com")
        assertEq(port    , "123")
        assertEq(pathname, "/hehe")
        assertEq(search  , "?q")
        assertEq(hash    , "#h")
        ()
      }
      assertEvalResult(sync.eval(test), ())
    }
  }
}
