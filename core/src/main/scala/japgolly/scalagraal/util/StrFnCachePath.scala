package japgolly.scalagraal.util

import japgolly.scalagraal.util.StrFnCachePath._
import scala.reflect.ClassTag

/** A specific subset of a type that will be cached.
  *
  * For example, you would have a `StrFnCachePath[Option[A]]` for the `None` case,
  * and another `StrFnCachePath[Option[A]]` for the `Some` case.
  *
  * @param isApplicable Whether a given value of `A` is within the subset of `A` represented by this path/instance.
  */
final case class StrFnCachePath[A](isApplicable: A => Boolean,
                                   newTokens   : () => Tokens[A]) {

  def xmap[B](f: A => B)(g: B => A): StrFnCachePath[B] =
    StrFnCachePath(isApplicable compose g, () => newTokens().xmap(f)(g))

  def widen[B >: A](implicit ct: ClassTag[A]): StrFnCachePath[B] = {
    val aClass = ct.runtimeClass
    StrFnCachePath(
      b => aClass.isInstance(b) && isApplicable(b.asInstanceOf[A]),
      () => newTokens().xmap(a => (a: B))(_.asInstanceOf[A]))
  }
}

object StrFnCachePath {

  final case class Tokens[A](tokens      : A,
                             replacements: List[Replacement[A]]) {

    def xmap[B](f: A => B)(g: B => A): Tokens[B] =
      Tokens(f(tokens), replacements.map(_.contramap(g)))
  }

  object Tokens {
    def const[A](value: A): Tokens[A] =
      Tokens(value, Nil)
  }

  final case class Replacement[A](tokenRegex  : String,
                                  replaceToken: String => Option[A => String]) {

    def contramap[B](f: B => A): Replacement[B] =
      copy(replaceToken = replaceToken(_).map(f.andThen))

    def contramapO[B](f: B => Option[A]): Replacement[B] =
      copy(replaceToken = s => replaceToken(s).map(fa => f(_).fold(s)(fa)))
  }

  private val always: Any => Boolean =
    _ => true

  def total[A](t: => Tokens[A]): StrFnCachePath[A] =
    StrFnCachePath(always, () => t)

  def const[A](value: A): StrFnCachePath[A] =
    const(always, value)

  def const[A](isApplicable: A => Boolean, value: A): StrFnCachePath[A] = {
    val t = Tokens.const(value)
    StrFnCachePath(isApplicable, () => t)
  }
}
