package japgolly.scalagraal.util

import scala.reflect.ClassTag
import japgolly.scalagraal.util.StringGenCachePath._

final case class StringGenCachePath[A](isApplicable: A => Boolean,
                                       newTokens   : () => Tokens[A]) {

  def xmap[B](f: A => B)(g: B => A): StringGenCachePath[B] =
    StringGenCachePath(isApplicable compose g, () => newTokens().xmap(f)(g))

  def widen[B >: A](implicit ct: ClassTag[B]): StringGenCachePath[B] = {
    val bClass = ct.runtimeClass
    StringGenCachePath(
      b => bClass.isInstance(b) && isApplicable(b.asInstanceOf[A]),
      () => newTokens().xmap(a => (a: B))(_.asInstanceOf[A]))
  }
}

object StringGenCachePath {

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

  // ===================================================================================================================

  private val always: Any => Boolean =
    _ => true

  def total[A](t: => Tokens[A]): StringGenCachePath[A] =
    StringGenCachePath(always, () => t)

  def const[A](value: A): StringGenCachePath[A] =
    const(always, value)

  def const[A](isApplicable: A => Boolean, value: A): StringGenCachePath[A] = {
    val t = Tokens.const(value)
    StringGenCachePath(isApplicable, () => t)
  }
}
