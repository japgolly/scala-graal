package japgolly.scalagraal

import org.graalvm.polyglot.Value

abstract class ExprBoilerplate private[scalagraal]() {

  protected final type X = AnyRef { type A = Unit }
  private[this] final val exprValueId = (a: Expr[Value]) => a

  protected def genericOpt[Z](params: Array[ExprParam[X]],
                              mkExprStr: Array[String] => String,
                              post: Expr[Value] => Z)
                             (implicit lang: Language): Array[X] => Z

  final class CompileDsl1[A](mkExpr: (String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A]): (A) => Z = {
      val ps = Array[ExprParam[_]](A).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0)), post)
      (a) => z(Array[Any](a).asInstanceOf[Array[X]])
    }
  }

  final def compile1[A](mkExpr: (String) => String): CompileDsl1[A] =
    new CompileDsl1(mkExpr)

  final def compileExpr1[A](mkExpr: (String) => String)(implicit lang: Language, A:ExprParam[A]): (A) => Expr[Value] =
    compile1[A](mkExpr)(exprValueId)

  final def apply1[A](mkExpr: (String) => String, a: A)(implicit lang: Language, A:ExprParam[A]): Expr[Value] =
    compileExpr1[A](mkExpr).apply(a)

  // ===================================================================================================================

  final class CompileDsl2[A,B](mkExpr: (String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): (A,B) => Z = {
      val ps = Array[ExprParam[_]](A,B).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1)), post)
      (a,b) => z(Array[Any](a,b).asInstanceOf[Array[X]])
    }
  }

  final def compile2[A,B](mkExpr: (String,String) => String): CompileDsl2[A,B] =
    new CompileDsl2(mkExpr)

  final def compileExpr2[A,B](mkExpr: (String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): (A,B) => Expr[Value] =
    compile2[A,B](mkExpr)(exprValueId)

  final def apply2[A,B](mkExpr: (String,String) => String, a: A, b: B)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): Expr[Value] =
    compileExpr2[A,B](mkExpr).apply(a,b)

  // ===================================================================================================================

  final class CompileDsl3[A,B,C](mkExpr: (String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): (A,B,C) => Z = {
      val ps = Array[ExprParam[_]](A,B,C).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2)), post)
      (a,b,c) => z(Array[Any](a,b,c).asInstanceOf[Array[X]])
    }
  }

  final def compile3[A,B,C](mkExpr: (String,String,String) => String): CompileDsl3[A,B,C] =
    new CompileDsl3(mkExpr)

  final def compileExpr3[A,B,C](mkExpr: (String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): (A,B,C) => Expr[Value] =
    compile3[A,B,C](mkExpr)(exprValueId)

  final def apply3[A,B,C](mkExpr: (String,String,String) => String, a: A, b: B, c: C)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): Expr[Value] =
    compileExpr3[A,B,C](mkExpr).apply(a,b,c)

  // ===================================================================================================================

  final class CompileDsl4[A,B,C,D](mkExpr: (String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): (A,B,C,D) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3)), post)
      (a,b,c,d) => z(Array[Any](a,b,c,d).asInstanceOf[Array[X]])
    }
  }

  final def compile4[A,B,C,D](mkExpr: (String,String,String,String) => String): CompileDsl4[A,B,C,D] =
    new CompileDsl4(mkExpr)

  final def compileExpr4[A,B,C,D](mkExpr: (String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): (A,B,C,D) => Expr[Value] =
    compile4[A,B,C,D](mkExpr)(exprValueId)

  final def apply4[A,B,C,D](mkExpr: (String,String,String,String) => String, a: A, b: B, c: C, d: D)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): Expr[Value] =
    compileExpr4[A,B,C,D](mkExpr).apply(a,b,c,d)

  // ===================================================================================================================

  final class CompileDsl5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): (A,B,C,D,E) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4)), post)
      (a,b,c,d,e) => z(Array[Any](a,b,c,d,e).asInstanceOf[Array[X]])
    }
  }

  final def compile5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String): CompileDsl5[A,B,C,D,E] =
    new CompileDsl5(mkExpr)

  final def compileExpr5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): (A,B,C,D,E) => Expr[Value] =
    compile5[A,B,C,D,E](mkExpr)(exprValueId)

  final def apply5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): Expr[Value] =
    compileExpr5[A,B,C,D,E](mkExpr).apply(a,b,c,d,e)

  // ===================================================================================================================

  final class CompileDsl6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): (A,B,C,D,E,F) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5)), post)
      (a,b,c,d,e,f) => z(Array[Any](a,b,c,d,e,f).asInstanceOf[Array[X]])
    }
  }

  final def compile6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String): CompileDsl6[A,B,C,D,E,F] =
    new CompileDsl6(mkExpr)

  final def compileExpr6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): (A,B,C,D,E,F) => Expr[Value] =
    compile6[A,B,C,D,E,F](mkExpr)(exprValueId)

  final def apply6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): Expr[Value] =
    compileExpr6[A,B,C,D,E,F](mkExpr).apply(a,b,c,d,e,f)

  // ===================================================================================================================

  final class CompileDsl7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): (A,B,C,D,E,F,G) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6)), post)
      (a,b,c,d,e,f,g) => z(Array[Any](a,b,c,d,e,f,g).asInstanceOf[Array[X]])
    }
  }

  final def compile7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String): CompileDsl7[A,B,C,D,E,F,G] =
    new CompileDsl7(mkExpr)

  final def compileExpr7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): (A,B,C,D,E,F,G) => Expr[Value] =
    compile7[A,B,C,D,E,F,G](mkExpr)(exprValueId)

  final def apply7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): Expr[Value] =
    compileExpr7[A,B,C,D,E,F,G](mkExpr).apply(a,b,c,d,e,f,g)

  // ===================================================================================================================

  final class CompileDsl8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): (A,B,C,D,E,F,G,H) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7)), post)
      (a,b,c,d,e,f,g,h) => z(Array[Any](a,b,c,d,e,f,g,h).asInstanceOf[Array[X]])
    }
  }

  final def compile8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String): CompileDsl8[A,B,C,D,E,F,G,H] =
    new CompileDsl8(mkExpr)

  final def compileExpr8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): (A,B,C,D,E,F,G,H) => Expr[Value] =
    compile8[A,B,C,D,E,F,G,H](mkExpr)(exprValueId)

  final def apply8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): Expr[Value] =
    compileExpr8[A,B,C,D,E,F,G,H](mkExpr).apply(a,b,c,d,e,f,g,h)

  // ===================================================================================================================

  final class CompileDsl9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): (A,B,C,D,E,F,G,H,I) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8)), post)
      (a,b,c,d,e,f,g,h,i) => z(Array[Any](a,b,c,d,e,f,g,h,i).asInstanceOf[Array[X]])
    }
  }

  final def compile9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String): CompileDsl9[A,B,C,D,E,F,G,H,I] =
    new CompileDsl9(mkExpr)

  final def compileExpr9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): (A,B,C,D,E,F,G,H,I) => Expr[Value] =
    compile9[A,B,C,D,E,F,G,H,I](mkExpr)(exprValueId)

  final def apply9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): Expr[Value] =
    compileExpr9[A,B,C,D,E,F,G,H,I](mkExpr).apply(a,b,c,d,e,f,g,h,i)

  // ===================================================================================================================

  final class CompileDsl10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): (A,B,C,D,E,F,G,H,I,J) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9)), post)
      (a,b,c,d,e,f,g,h,i,j) => z(Array[Any](a,b,c,d,e,f,g,h,i,j).asInstanceOf[Array[X]])
    }
  }

  final def compile10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String): CompileDsl10[A,B,C,D,E,F,G,H,I,J] =
    new CompileDsl10(mkExpr)

  final def compileExpr10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): (A,B,C,D,E,F,G,H,I,J) => Expr[Value] =
    compile10[A,B,C,D,E,F,G,H,I,J](mkExpr)(exprValueId)

  final def apply10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): Expr[Value] =
    compileExpr10[A,B,C,D,E,F,G,H,I,J](mkExpr).apply(a,b,c,d,e,f,g,h,i,j)

  // ===================================================================================================================

  final class CompileDsl11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): (A,B,C,D,E,F,G,H,I,J,K) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10)), post)
      (a,b,c,d,e,f,g,h,i,j,k) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k).asInstanceOf[Array[X]])
    }
  }

  final def compile11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl11[A,B,C,D,E,F,G,H,I,J,K] =
    new CompileDsl11(mkExpr)

  final def compileExpr11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): (A,B,C,D,E,F,G,H,I,J,K) => Expr[Value] =
    compile11[A,B,C,D,E,F,G,H,I,J,K](mkExpr)(exprValueId)

  final def apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): Expr[Value] =
    compileExpr11[A,B,C,D,E,F,G,H,I,J,K](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k)

  // ===================================================================================================================

  final class CompileDsl12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l).asInstanceOf[Array[X]])
    }
  }

  final def compile12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl12[A,B,C,D,E,F,G,H,I,J,K,L] =
    new CompileDsl12(mkExpr)

  final def compileExpr12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Expr[Value] =
    compile12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr)(exprValueId)

  final def apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): Expr[Value] =
    compileExpr12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l)

  // ===================================================================================================================

  final class CompileDsl13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m).asInstanceOf[Array[X]])
    }
  }

  final def compile13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl13[A,B,C,D,E,F,G,H,I,J,K,L,M] =
    new CompileDsl13(mkExpr)

  final def compileExpr13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Expr[Value] =
    compile13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr)(exprValueId)

  final def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): Expr[Value] =
    compileExpr13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m)

  // ===================================================================================================================

  final class CompileDsl14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n).asInstanceOf[Array[X]])
    }
  }

  final def compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] =
    new CompileDsl14(mkExpr)

  final def compileExpr14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Expr[Value] =
    compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr)(exprValueId)

  final def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): Expr[Value] =
    compileExpr14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

  // ===================================================================================================================

  final class CompileDsl15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o).asInstanceOf[Array[X]])
    }
  }

  final def compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] =
    new CompileDsl15(mkExpr)

  final def compileExpr15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Expr[Value] =
    compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr)(exprValueId)

  final def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): Expr[Value] =
    compileExpr15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

  // ===================================================================================================================

  final class CompileDsl16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p).asInstanceOf[Array[X]])
    }
  }

  final def compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] =
    new CompileDsl16(mkExpr)

  final def compileExpr16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Expr[Value] =
    compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr)(exprValueId)

  final def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): Expr[Value] =
    compileExpr16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

  // ===================================================================================================================

  final class CompileDsl17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q).asInstanceOf[Array[X]])
    }
  }

  final def compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] =
    new CompileDsl17(mkExpr)

  final def compileExpr17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Expr[Value] =
    compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr)(exprValueId)

  final def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): Expr[Value] =
    compileExpr17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

  // ===================================================================================================================

  final class CompileDsl18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r).asInstanceOf[Array[X]])
    }
  }

  final def compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] =
    new CompileDsl18(mkExpr)

  final def compileExpr18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Expr[Value] =
    compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr)(exprValueId)

  final def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): Expr[Value] =
    compileExpr18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

  // ===================================================================================================================

  final class CompileDsl19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s).asInstanceOf[Array[X]])
    }
  }

  final def compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] =
    new CompileDsl19(mkExpr)

  final def compileExpr19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Expr[Value] =
    compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr)(exprValueId)

  final def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): Expr[Value] =
    compileExpr19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

  // ===================================================================================================================

  final class CompileDsl20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t).asInstanceOf[Array[X]])
    }
  }

  final def compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] =
    new CompileDsl20(mkExpr)

  final def compileExpr20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Expr[Value] =
    compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr)(exprValueId)

  final def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): Expr[Value] =
    compileExpr20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

  // ===================================================================================================================

  final class CompileDsl21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u).asInstanceOf[Array[X]])
    }
  }

  final def compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] =
    new CompileDsl21(mkExpr)

  final def compileExpr21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Expr[Value] =
    compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr)(exprValueId)

  final def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): Expr[Value] =
    compileExpr21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

  // ===================================================================================================================

  final class CompileDsl22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {
    def apply[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20),e(21)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v).asInstanceOf[Array[X]])
    }
  }

  final def compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): CompileDsl22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] =
    new CompileDsl22(mkExpr)

  final def compileExpr22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Expr[Value] =
    compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr)(exprValueId)

  final def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): Expr[Value] =
    compileExpr22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
}