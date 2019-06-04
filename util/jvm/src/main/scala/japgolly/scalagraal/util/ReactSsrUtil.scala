package japgolly.scalagraal.util

import japgolly.scalagraal._
import GraalJs._

object ReactSsrUtil {

  object Setup {
    val postReact = Expr("window = {console: console, navigator: {userAgent:''}}")

    def apply(loadReact: Expr[Any]*): Expr[Unit] =
      for {
        _ <- Expr.stdlibCosequenceDiscard(loadReact)
        _ <- postReact
      } yield ()
  }

}
