import sbt._

object GenExprBoilerplate {

  def apply(outputDir: File): File = {

    val Name = "ExprBoilerplate"

    val groups =
      (1 to 22).map { n =>
        val _ABC = (1 to n).map(_ + 64).map(_.toChar)
        val ABC = _ABC.mkString(",")
        val abc = (1 to n).map(_ + 96).map(_.toChar).mkString(",")
        val `$a,$b,$c` = (1 to n).map(_ + 96).map("$" + _.toChar).mkString(",")
        val Types = _ABC.map(A => s"${A.toLower}: $A").mkString(", ")
        val Params = _ABC.map(A => s"$A:ExprParam[$A]").mkString(", ")
        val Strings = List.fill(n)("String").mkString(",")
        val es = (0 until n).map(i => s"e($i)").mkString(",")

        s"""
           |  final def apply$n[$ABC](mkExpr: ($Strings) => String): Apply$n[$ABC] =
           |    new Apply$n(mkExpr)
           |
           |  final def apply$n[$ABC](mkExpr: ($Strings) => String, $Types)(implicit lang: Language, $Params): Expr[Value] =
           |    apply$n[$ABC](mkExpr).apply($abc)
           |
           |  final def fn$n[$ABC](fnName: String): Apply$n[$ABC] =
           |    apply$n(($abc) => s"$$fnName(${`$a,$b,$c`})")
           |
           |  final def fn$n[$ABC](fnName: String, $Types)(implicit lang: Language, $Params): Expr[Value] =
           |    fn$n[$ABC](fnName).apply($abc)
           |
           |  final class Apply$n[$ABC](mkExpr: ($Strings) => String) {
           |
           |    @inline def apply($Types)(implicit lang: Language, $Params): Expr[Value] =
           |      compile.apply($abc)
           |
           |    def compile(implicit lang: Language, $Params): ($ABC) => Expr[Value] =
           |      compile[Expr[Value]](exprValueId)
           |
           |    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, $Params): ($ABC) => Z = {
           |      val ps = Array[ExprParam[_]]($ABC).asInstanceOf[Array[ExprParam[X]]]
           |      val z = genericOpt(ps, e => mkExpr($es), post)
           |      ($abc) => z(Array[Any]($abc).asInstanceOf[Array[X]])
           |    }
           |  }
         """.stripMargin.trim.replaceFirst("^", "  ")
      }

    val sep = s"\n  // ${"=" * 115}\n\n"

    val content =
      s"""
         |package japgolly.scalagraal
         |
         |import org.graalvm.polyglot.Value
         |
         |abstract class $Name private[scalagraal]() {
         |
         |  protected final type X = AnyRef { type A = Unit }
         |  private[this] final val exprValueId = (a: Expr[Value]) => a
         |
         |  protected def genericOpt[Z](params: Array[ExprParam[X]],
         |                              mkExprStr: Array[String] => String,
         |                              post: Expr[Value] => Z)
         |                             (implicit lang: Language): Array[X] => Z
         |$sep${groups.mkString("\n" + sep)}
         |}
        """.stripMargin.trim

    val file = (outputDir / "japgolly" / "scalagraal" / s"$Name.scala").asFile
    IO.write(file, content)
    println(s"Generated ${file.getAbsolutePath}")
    file
  }
}
