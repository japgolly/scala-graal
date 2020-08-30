package japgolly.scalagraal.util

import cats.{Functor, Id}
import cats.instances.either._
import cats.syntax.functor._
import japgolly.scalagraal.Expr
import scala.annotation.tailrec

/** Caches `A => F[String]` functions and makes them super-fast by executing once per path,
  * optimising and caching the result, and then using it as template for all subsequent calls.
  *
  * "Paths" are independent from each other and are defined by [[StrFnCacheParam]] instances. There is typically
  * one path per sum type, for example `Option` has two paths: `None` and `Some[A]`; `String` only has one which is
  * itself.
  *
  * Values (once in a path) must be completely opaque and not used to affect function logic or conditionality.
  * For example, a function that takes a name and prints it a few times is fine, but a function which branched
  * according to string length, or converted the string to uppercase would be broken by this cache because the caching
  * logic doesn't branch according to string length, and the caching logic doesn't convert strings to uppercase. You
  * can still have your super-fast cake and eat it too by creating your own customised [[StrFnCacheParam]] instance
  * that includes any value modification or branching you need. In such cases it would be wise to cross-compile the
  * logic rather than duplicate it, and/or modify your function (or React component) to accept inputs that have already
  * been processed.
  *
  * @since 1.1.0
  */
object StrFnCache {
  import StrFnCachePath._

  def id[A](f: A => String)(implicit p: StrFnCacheParam[A]): A => String =
    poly[Id, A](f)

  def apply[A](f: A => Expr.Result[String])(implicit p: StrFnCacheParam[A]): A => Expr.Result[String] =
    poly(f)

  def poly[F[_], A](f: A => F[String])(implicit p: StrFnCacheParam[A], F: Functor[F]): A => F[String] =
    p.paths match {

      case Nil =>
        f

      case p :: Nil =>
        cachePath(p.newTokens(), f)

      case _ =>
        type Result = A => F[String]
        type Cache = (StrFnCachePath[A], Result)
        val caches = p.paths.map[Cache](p => p -> cachePath(p.newTokens(), f))
        a => {
          @tailrec
          def go(cs: List[Cache]): Result =
            cs match {
              case c :: ct =>
                if (c._1.isApplicable(a))
                  c._2
                else
                  go(ct)
              case Nil =>
                f
            }
          go(caches)(a)
        }
    }

  private def cachePath[F[_], A](t: Tokens[A], f: A => F[String])(implicit F: Functor[F]): A => F[String] = {

    val fFn: F[A => String] =
      f(t.tokens).map { templateWithTokens =>
        if (templateWithTokens eq null)
          _ => null
        else if (t.replacements.isEmpty)
          _ => templateWithTokens
        else {

          val regex = {
            val i = t.replacements.map(_.tokenRegex).mkString("|")
            val r = s"(?<=$i)|(?=$i)"
            r.r
          }

          def makeFragFn(frag: String): (A, StringBuilder) => Unit =
            t.replacements.iterator.flatMap(_.replaceToken(frag)).nextOption() match {
              case Some(f) => (i, sb) => sb.append(f(i)); ()
              case None    => (_, sb) => sb.append(frag); ()
            }

          val fragFns =
            regex
              .split(templateWithTokens)
              .iterator
              .filter(_.nonEmpty)
              .map(makeFragFn)
              .toArray

          fragFns.length match {
            case 0 =>
              _ => ""

            case 1 =>
              val ff = fragFns(0)
              a => {
                val sb = new StringBuilder
                ff(a, sb)
                sb.toString()
              }

            case len =>
              a => {
                val sb = new StringBuilder
                var i = 0
                while (i < len) {
                  fragFns(i)(a, sb)
                  i += 1
                }
                sb.toString()
              }
          }
        }
      }

    i => fFn.map(_(i))
  }

}
