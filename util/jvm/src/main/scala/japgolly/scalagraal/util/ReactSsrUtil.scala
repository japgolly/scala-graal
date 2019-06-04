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
}
