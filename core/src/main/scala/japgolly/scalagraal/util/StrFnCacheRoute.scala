package japgolly.scalagraal.util

import java.util.concurrent.ConcurrentHashMap
import java.util.function.{Function => J8Fn}

trait StrFnCacheRoute[A] {
  def total[B](f: A => B): A => B
  def whitelist[B](f: A => B)(whitelistedValues: A*): A => Option[B]

  final def xmap[B](f: A => B)(g: B => A): StrFnCacheRoute[B] =
    StrFnCacheRoute.apply1(f)(g)(this)
}

object StrFnCacheRoute {

  def useMap[A <: AnyRef]: StrFnCacheRoute[A] =
    new StrFnCacheRoute[A] {

      override def total[B](f: A => B): A => B = {
        val cache = new ConcurrentHashMap[A, B](32)
        val mf    = new J8Fn[A, B] { override def apply(a: A): B = f(a) }
        a => cache.computeIfAbsent(a, mf)
      }

      override def whitelist[B](f: A => B)(whitelistedValues: A*) =
        whitelistedValues.foldLeft(Map.empty[A, B])((m, a) => m.updated(a, f(a))).get
    }

  implicit val string: StrFnCacheRoute[String] =
    useMap

  def apply1[A, Z](f: A => Z)(g: Z => A)(implicit r: StrFnCacheRoute[A]): StrFnCacheRoute[Z] =
    new StrFnCacheRoute[Z] {
      override def total[B](h: Z => B): Z => B = {
        val m = r.total(h compose f)
        m compose g
      }

      override def whitelist[B](h: Z => B)(whitelistedValues: Z*): Z => Option[B] = {
        val m = r.whitelist(h compose f)(whitelistedValues.map(g): _*)
        m compose g
      }
    }

}
