package japgolly.scalagraal.util

import java.util.UUID
import japgolly.scalagraal.util.StrFnCachePath._
import java.util.regex.Pattern
import scala.reflect.ClassTag

final case class StrFnCacheParam[A](paths: List[StrFnCachePath[A]]) {

  def mapPaths[B](f: StrFnCachePath[A] => StrFnCachePath[B]): StrFnCacheParam[B] =
    StrFnCacheParam(paths map f)

  def xmap[B](f: A => B)(g: B => A): StrFnCacheParam[B] =
    mapPaths(_.xmap(f)(g))

  def widen[B >: A](implicit ct: ClassTag[A]): StrFnCacheParam[B] =
    mapPaths(_.widen[B])
}

object StrFnCacheParam extends StrFnCacheParamBoilerplate {

  def disabled[A]: StrFnCacheParam[A] =
    apply(Nil)

  def const[A](value: A): StrFnCacheParam[A] =
    apply(StrFnCachePath.const(value) :: Nil)

  def enum[A](allEnumValues: A*): StrFnCacheParam[A] =
    apply(allEnumValues.iterator.map(a => StrFnCachePath.const[A](a == _, a)).toList)

  def viaToken[A](tokenFn: () => A)
                 (toString: A => String,
                  regexMod: String => String = identity): StrFnCacheParam[A] = {
    val path = StrFnCachePath.total[A] {
      val token    = tokenFn()
      val tokenStr = toString(token)
      val regex    = s"(?:${regexMod(Pattern.quote(tokenStr))})"
      val replace  = (s: String) => Option.when(s == tokenStr)(toString)
      Tokens(token, Replacement(regex, replace) :: Nil)
    }
    apply(path :: Nil)
  }

  def apply1[A, Z](f: A => Z)(g: Z => A)(implicit t: StrFnCacheParam[A]): StrFnCacheParam[Z] =
    t.xmap(f)(g)

  def numericRegexMod(r: String): String =
    if (r.startsWith("-"))
      s"$r(?!\\d)"
    else
      s"(?<!\\d|(?<!\\d)-)$r(?!\\d)"

  def char(token: Char = '\uE666', toString: Char => String = _.toString): StrFnCacheParam[Char] =
    viaToken(() => token)(toString)

  def short(token: Short = Short.MinValue, toString: Short => String = _.toString): StrFnCacheParam[Short] =
    viaToken(() => token)(toString, numericRegexMod)

  def int(token: Int = Int.MinValue, toString: Int => String = _.toString): StrFnCacheParam[Int] =
    viaToken(() => token)(toString, numericRegexMod)

  def long(token: Long = Long.MinValue, toString: Long => String = _.toString): StrFnCacheParam[Long] =
    viaToken(() => token)(toString, numericRegexMod)

  implicit val string: StrFnCacheParam[String] = {
    val tokenFn = () => {
      val id = UUID.randomUUID()
        .toString
        .replace("-", "")
        .map(c => (c.toInt + 58982).toChar) // unicode private use area
        .mkString
      s"[scalagraal:$id]"
    }
    viaToken(tokenFn)(identity)
  }

  implicit val boolean: StrFnCacheParam[Boolean] =
    enum(true, false)

  implicit val unit: StrFnCacheParam[Unit] =
    const(())

  implicit def option[A](implicit underlying: StrFnCacheParam[A]): StrFnCacheParam[Option[A]] =
    divide2(
      const(None),
      underlying.xmap(Some(_))(_.value))

  implicit def either[A, B](implicit l: StrFnCacheParam[A], r: StrFnCacheParam[B]): StrFnCacheParam[Either[A, B]] =
    divide2(
      l.xmap(Left(_))(_.value),
      r.xmap(Right(_))(_.value))

}
