package japgolly.scalagraal.util

import utest._

object WindowLocationTest extends TestSuite {

  private def assertParses(url: String, expect: WindowLocation): Unit = {
    val actual = WindowLocation.parse(url).get
    assert(actual == expect)
  }

  override def tests = Tests {

    * - assertParses("http://localhost", WindowLocation(
          href     = "http://localhost",
          origin   = "http://localhost",
          protocol = "http:",
          hostname = "localhost",
          port     = "",
          pathname = "",
          search   = "",
          hash     = ""))

    * - assertParses("https://ah.sub.localhost:8080?x", WindowLocation(
          href     = "https://ah.sub.localhost:8080?x",
          origin   = "https://ah.sub.localhost:8080",
          protocol = "https:",
          hostname = "ah.sub.localhost",
          port     = "8080",
          pathname = "",
          search   = "?x",
          hash     = ""))

    * - assertParses("https://what.com.au:123", WindowLocation(
          href     = "https://what.com.au:123",
          origin   = "https://what.com.au:123",
          protocol = "https:",
          hostname = "what.com.au",
          port     = "123",
          pathname = "",
          search   = "",
          hash     = ""))

    * - assertParses("https://what.com.au:123/", WindowLocation(
          href     = "https://what.com.au:123/",
          origin   = "https://what.com.au:123",
          protocol = "https:",
          hostname = "what.com.au",
          port     = "123",
          pathname = "/",
          search   = "",
          hash     = ""))

    * - assertParses("https://what.com.au/?a#b?c#d", WindowLocation(
          href     = "https://what.com.au/?a#b?c#d",
          origin   = "https://what.com.au",
          protocol = "https:",
          hostname = "what.com.au",
          port     = "",
          pathname = "/",
          search   = "?a",
          hash     = "#b?c#d"))

    * - assertParses("https://what.com.au/x/y/#a?b#c?d", WindowLocation(
          href     = "https://what.com.au/x/y/#a?b#c?d",
          origin   = "https://what.com.au",
          protocol = "https:",
          hostname = "what.com.au",
          port     = "",
          pathname = "/x/y/",
          search   = "",
          hash     = "#a?b#c?d"))
  }
}
