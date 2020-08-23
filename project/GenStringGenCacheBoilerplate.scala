import sbt._

object GenStringGenCacheBoilerplate {

  def apply(outputDir: File): File = {

    val groups =
      (2 to 22).map { n =>
        def up(i: Int) = (i + 65).toChar
        val _ABC = (0 until n).map(up)
        val ABC = _ABC.mkString(",")
        def repeat(s: String, sep: String = ", ") = (0 until n).map(i => s.replace("_?", "_"+(i+1)).replace('?', up(i))).mkString(sep)
        s"""
           |  def apply$n[$ABC,Z](f: ($ABC) => Z)(g: Z => ($ABC))(implicit t: StringGenCache[($ABC)]): StringGenCache[Z] =
           |    t.xmap(x => f(${repeat("x._?")}))(g)
           |
           |  implicit def tuple$n[$ABC](${repeat("c?: StringGenCache[?]")}): StringGenCache[($ABC)] = {
           |    type Z = ($ABC)
           |    val paths =
           |      for {
           |        ${repeat("p? <- c?.paths", "\n        ")}
           |      } yield
           |        StringGenCachePath[Z](
           |          t => ${repeat("p?.isApplicable(t._?)", " && ")},
           |          () => {
           |            ${repeat("val t? = p?.newTokens()", "\n            ")}
           |            val tokens = (${repeat("t?.tokens")})
           |            var replacements = List.empty[Replacement[Z]]
           |            ${repeat("t?.replacements.foreach(r => replacements ::= r.contramap[Z](_._?))", "\n            ")}
           |            Tokens(tokens, replacements)
           |          }
           |        )
           |    apply(paths)
           |  }
         """.stripMargin.trim.replaceFirst("^", "  ")
      }

    val Name = "StringGenCacheBoilerplate"

    val sep = s"\n  // ${"=" * 115}\n\n"

    val content =
      s"""
         |package japgolly.scalagraal.util
         |
         |import japgolly.scalagraal.util.StringGenCachePath._
         |
         |abstract class $Name private[util]() {
         |
         |  def apply[A](paths: List[StringGenCachePath[A]]): StringGenCache[A]
         |
         |$sep${groups.mkString("\n" + sep)}
         |}
        """.stripMargin.trim

    val file = (outputDir / "japgolly" / "scalagraal" / "util" / s"$Name.scala").asFile
    IO.write(file, content)
    println(s"Generated ${file.getAbsolutePath}")
    file
  }
}
