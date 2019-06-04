package japgolly.scalagraal.util

import cats.{Functor, Id}
import cats.syntax.functor._
import java.util.UUID

/** Takes a potentially slow `String* => String` function and makes it super fast by executing it once,
  * turning the result into a template, then using the template for all subsequent calls.
  *
  * This assumes provided functions are pure.
  */
object Template extends TemplateBoilerplate {

  final class Param[A](val fromStr: String => A, val toStr: A => String)

  object Param {
    def apply[A](fromStr: String => A)(toStr: A => String): Param[A] =
      new Param(fromStr, toStr)

    implicit val string: Param[String] = {
      val id = (s: String) => s
      apply(id)(id)
    }
  }

  // ===================================================================================================================

  override protected def compileGeneric[F[_]: Functor](arity: Int, f: Array[String] => F[String]): F[Array[String] => String] = {
    val ids = Array.fill(arity)(newId())

    f(ids).map { templateWithIds =>
      if (templateWithIds eq null)
        _ => null
      else if (!ids.exists(templateWithIds.contains))
        _ => templateWithIds
      else {

        val regex = {
          val i = ids.mkString("|")
          val r = s"(?=$i)|(?<=$i)"
          r.r
        }

        def makeFragFn(frag: String): (Array[String], StringBuilder) => Unit =
          ids.indexOf(frag) match {
            case -1 => (_, sb) => sb.append(frag); ()
            case i  => (a, sb) => sb.append(a(i)); ()
          }

        val fragFns =
          regex
            .split(templateWithIds)
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
  }

  private def newId(): String =
    "\u0001" + UUID.randomUUID().toString.replace("-", " ") + "\u0002"
}
