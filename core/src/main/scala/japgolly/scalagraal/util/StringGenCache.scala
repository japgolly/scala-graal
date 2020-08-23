package japgolly.scalagraal.util

import cats.Functor
import cats.syntax.functor._
import java.util.UUID
import japgolly.scalagraal.util.StringGenCachePath._
import scala.annotation.tailrec
import scala.reflect.ClassTag

final case class StringGenCache[A](paths: List[StringGenCachePath[A]]) {

  def mapPaths[B](f: StringGenCachePath[A] => StringGenCachePath[B]): StringGenCache[B] =
    StringGenCache(paths map f)

  def xmap[B](f: A => B)(g: B => A): StringGenCache[B] =
    StringGenCache(paths.map(_.xmap(f)(g)))

  def cache[F[_]](f: A => F[String])(implicit F: Functor[F]): A => F[String] =
    paths match {
      case Nil => f
      case p :: Nil => cachePath(p.newTokens(), f)
      case _ =>
        type Result = A => F[String]
        type Cache = (StringGenCachePath[A], Result)
        val caches = paths.map[Cache](p => p -> cachePath(p.newTokens(), f))
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

  private def cachePath[F[_]](t: Tokens[A], f: A => F[String])(implicit F: Functor[F]): A => F[String] = {

    val fFn: F[A => String] =
      f(t.tokens).map { templateWithTokens =>
        if (templateWithTokens eq null)
          _ => null
        else if (t.replacements.isEmpty)
          _ => templateWithTokens
        else {

          val regex = {
            val i = t.replacements.map(_.tokenRegex).mkString("|")
            val r = s"(?=$i)|(?<=$i)"
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

// =====================================================================================================================

object StringGenCache extends StringGenCacheBoilerplate {

  def disabled[A]: StringGenCache[A] =
    apply(Nil)

  def const[A](value: A): StringGenCache[A] =
    apply(StringGenCachePath.const(value) :: Nil)

  def enum[A](allEnumValues: A*): StringGenCache[A] =
    apply(allEnumValues.iterator.map(a => StringGenCachePath.const[A](a == _, a)).toList)

  def sum[A: ClassTag](parts: StringGenCache[_ <: A]*): StringGenCache[A] =
    apply(parts.iterator.flatMap(_.paths.map(_.widen[A])).toList)

  implicit val string: StringGenCache[String] = {
    val strIdentity: String => String = s => s
    val path = StringGenCachePath.total[String] {
      val token   = s"\u0001scalagraal_${UUID.randomUUID().toString.replace("-", "")}\u0002"
      val regex   = s"(?:$token)"
      val replace = (s: String) => Option.when(s == token)(strIdentity)
      Tokens(token, Replacement(regex, replace) :: Nil)
    }
    apply(path :: Nil)
  }

  implicit val boolean: StringGenCache[Boolean] =
    enum(true, false)

  implicit val unit: StringGenCache[Unit] =
    const(())

  implicit def option[A](underlying: StringGenCache[A]): StringGenCache[Option[A]] =
    sum(
      const(None),
      underlying.xmap(Some(_))(_.value))

  implicit def either[A, B](l: StringGenCache[A], r: StringGenCache[B]): StringGenCache[Either[A, B]] =
    sum(
      l.xmap(Left(_))(_.value),
      r.xmap(Right(_))(_.value))

}
