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
  }
}
