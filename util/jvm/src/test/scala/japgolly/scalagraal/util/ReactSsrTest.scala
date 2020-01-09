package japgolly.scalagraal.util

import japgolly.scalagraal._
import java.time.Year
import utest._
import TestUtil._

object ReactSsrTest extends TestSuite {

  private lazy val setup =
    ReactSsr.Setup(
      Expr.requireFileOnClasspath("react.production.min.js"),
      Expr.requireFileOnClasspath("react-dom-server.browser.production.min.js"),
    )

  override def tests = Tests {

    "renderToStaticMarkup" - {
      val expr = setup >> ReactSsr.renderToStaticMarkup("React.createElement('div', null, 'hehe')")
      val r = sync.eval(expr)
      assertEvalResult(r, "<div>hehe</div>")
    }

    "renderToString" - {
      val expr = setup >> ReactSsr.renderToString("React.createElement('div', null, 'hehe')")
      val r = sync.eval(expr)
      assertEvalResult(r, """<div data-reactroot="">hehe</div>""")
    }

    "setUrl" - {
      val test = for {
        _        <- setup
        _        <- ReactSsr.setUrl("http://blah.com:123/hehe?q#h")
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

    "setUserAgent" - {
      val expr = for {
        _ <- setup
        _ <- ReactSsr.setUserAgent("lol")
        r <- Expr("window.navigator.userAgent").asString
      } yield r
      assertEvalResult(sync.eval(expr), "lol")
    }

    "navigator" - {
      val x = sync.eval(setup >> Expr("JSON.stringify(window.navigator)").asString)
      assert(x.isRight)
      x.getOrElse(???)
    }

    "sample" - {
      val expr = for {
        _ <- setup
        _ <- Expr.requireFileOnClasspath("sample-sjr-spa.js")
        _ <- ReactSsr.setUrl("https://shipreq.com")
        r <- Expr("A.comp2()").asString
      } yield r
      val r = sync.eval(expr)
      val y = Year.now().getValue
      val e = """<div style="display:flex;flex-direction:column;align-items:stretch;min-height:100%" data-reactroot=""><header style="width:100%;display:flex;align-items:flex-start"><div style="width:5em;padding:0.5em"></div><div style="flex:1;text-align:center;padding-top:0.2em"><a href="https://shipreq.com/login">Login</a><span style="padding:0 1em;color:#aaa">|</span><a href="https://shipreq.com/register">Register</a></div><div style="width:5em;padding:0.5em"></div></header><main style="flex:1;padding-bottom:2em"><div style="max-width:144ex;margin:1em auto 0 auto;padding:0 4vw"><div><img src="/s/878536f0a4ad0029794669f88dbf5f35.svg" alt="Xxxx" style="width:80%"/></div><div class="s_____q_2">Mauris semper sem quis arcu pellentesque ultrices.</div><div style="display:flex;margin-top:5em"><div class="s_____q_4"><div style="color:hsl(209, 100%, 15%);font-size:1.6em;line-height:1.3em">Lorem ipsum dolor sit amet, consectetur adipiscing elit.<br/>Aliquam enim lacus, viverra eu iaculis nec,<br/>viverra eget ipsum.</div><div style="color:hsl(207, 100%, 5%);font-size:1.15em;margin-top:3em;line-height:1.5em">Fusce pulvinar risus turpis, nec faucibus leo tristique sit amet?<br/>Duis nisi ex !<span class="s_____q_3"></span></div></div><div style="width:44ex;max-width:60%"><div style="background:#e5edf3;border:solid 1px #c0d6e9;box-shadow:0 2px 4px 0 rgba(192, 214, 233, .15), 0 2px 10px 0 rgba(192, 214, 233, .25);border-radius:.28571429rem;padding:1em"><div class="ui form"><div class="field"><div class="ui input left icon"><input type="text" placeholder="Your name" value=""/><i class="icon user"></i></div></div><div class="field"><div class="ui input left icon"><input type="text" placeholder="Your email address" value=""/><i class="icon mail"></i></div></div><div class="field"><textarea rows="12" placeholder="Quisque dictum ante vel risus suscipit consequat?"></textarea></div><div class="field" style="text-align:center"><div class="ui checkbox"><input type="checkbox" checked=""/><label style="cursor:pointer">Nunc sit amet magna</label></div></div><div class="field" style="text-align:center"><button class="ui button   blue large" style="margin-right:0">Lorem Ipsum</button></div></div></div></div></div></div></main><footer style="font-size:0.85rem;padding-top:0.1em;padding-bottom:0.1em;background:#edeeef;border-top:1px solid rgba(34,36,38,.15);text-align:center"><span style="color:#888">© 2013-2019 Xxxx</span><span style="padding:0 1em;color:#aaa">|</span><a href="https://shipreq.com/terms">Terms</a><span style="padding:0 1em;color:#aaa">|</span><a href="https://shipreq.com/privacy">Privacy</a></footer></div>"""
      assertEvalResult(r, e.replace("2019", y.toString))
    }
  }
}
