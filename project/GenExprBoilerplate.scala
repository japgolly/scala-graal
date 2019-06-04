import sbt._

object GenExprBoilerplate {

  def apply(outputDir: File): File = {

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
           |  final class CompileDsl$n[$ABC](mkExpr: ($Strings) => String) {
           |    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, $Params): ($ABC) => Z = {
           |      val ps = Array[ExprParam[_]]($ABC).asInstanceOf[Array[ExprParam[X]]]
           |      val z = genericOpt(ps, e => mkExpr($es), post)
           |      ($abc) => z(Array[Any]($abc).asInstanceOf[Array[X]])
           |    }
           |  }
           |
           |  final def compile$n[$ABC](mkExpr: ($Strings) => String): CompileDsl$n[$ABC] =
           |    new CompileDsl$n(mkExpr)
           |
           |  final def compileExpr$n[$ABC](mkExpr: ($Strings) => String)(implicit lang: Language, $Params): ($ABC) => Expr[Value] =
           |    compile$n[$ABC](mkExpr)(exprValueId)
           |
           |  final def apply$n[$ABC](mkExpr: ($Strings) => String, $Types)(implicit lang: Language, $Params): Expr[Value] =
           |    compileExpr$n[$ABC](mkExpr).apply($abc)
           |
           |  final def compileFnCall$n[$ABC](fnName: String): CompileDsl$n[$ABC] =
           |    compile$n[$ABC](($abc) => s"$$fnName(${`$a,$b,$c`})")
           |
           |  final def callFn$n[$ABC](fnName: String, $Types)(implicit lang: Language, $Params): Expr[Value] =
           |    compileFnCall$n[$ABC](fnName)(exprValueId).apply($abc)
         """.stripMargin.trim.replaceFirst("^", "  ")
      }

    val Name = "ExprBoilerplate"

    val sep = s"\n  // ${"=" * 115}\n\n"

    val content =
      s"""
         |package japgolly.scalagraal
         |package boilerplate
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

    val file = (outputDir / "japgolly" / "scalagraal" / "boilerplate" / s"$Name.scala").asFile
    IO.write(file, content)
    println(s"Generated ${file.getAbsolutePath}")
    file
  }
}
