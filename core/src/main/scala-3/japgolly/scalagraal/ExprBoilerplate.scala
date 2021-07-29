package japgolly.scalagraal

import org.graalvm.polyglot.Value

abstract class ExprBoilerplate private[scalagraal]() {

  protected final type X = AnyRef { type A = Unit }
  private[this] final val exprValueId = (a: Expr[Value]) => a

  protected def genericOpt[Z](params: Array[ExprParam[X]],
                              mkExprStr: Array[String] => String,
                              post: Expr[Value] => Z)
                             (implicit lang: Language): Array[X] => Z

  // ===================================================================================================================

  final def apply1[A](mkExpr: (String) => String, a: A)(implicit lang: Language, A:ExprParam[A]): Expr[Value] =
    apply1[A](mkExpr).apply(a)

  final def apply1[A](mkExpr: (String) => String): Apply1[A] =
    new Apply1(mkExpr)

  final def fn1[A](fnName: String): Apply1[A] =
    apply1((a) => s"$fnName($a)")

  final def fn1[A](fnName: String, a: A)(implicit lang: Language, A:ExprParam[A]): Expr[Value] =
    fn1[A](fnName).apply(a)

  final class Apply1[A](mkExpr: (String) => String) {

    @inline def apply(a: A)(implicit lang: Language, A:ExprParam[A]): Expr[Value] =
      compile.apply(a)

    def compile(implicit lang: Language, A:ExprParam[A]): (A) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A]): (A) => Z = {
      val ps = Array[ExprParam[_]](A).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0)), post)
      (a) => z(Array[Any](a).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply2[A,B](mkExpr: (String,String) => String, a: A, b: B)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): Expr[Value] =
    apply2[A,B](mkExpr).apply(a,b)

  final def apply2[A,B](mkExpr: (String,String) => String): Apply2[A,B] =
    new Apply2(mkExpr)

  final def fn2[A,B](fnName: String): Apply2[A,B] =
    apply2((a,b) => s"$fnName($a,$b)")

  final def fn2[A,B](fnName: String, a: A, b: B)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): Expr[Value] =
    fn2[A,B](fnName).apply(a,b)

  final class Apply2[A,B](mkExpr: (String,String) => String) {

    @inline def apply(a: A, b: B)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): Expr[Value] =
      compile.apply(a,b)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): (A,B) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B]): (A,B) => Z = {
      val ps = Array[ExprParam[_]](A,B).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1)), post)
      (a,b) => z(Array[Any](a,b).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply3[A,B,C](mkExpr: (String,String,String) => String, a: A, b: B, c: C)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): Expr[Value] =
    apply3[A,B,C](mkExpr).apply(a,b,c)

  final def apply3[A,B,C](mkExpr: (String,String,String) => String): Apply3[A,B,C] =
    new Apply3(mkExpr)

  final def fn3[A,B,C](fnName: String): Apply3[A,B,C] =
    apply3((a,b,c) => s"$fnName($a,$b,$c)")

  final def fn3[A,B,C](fnName: String, a: A, b: B, c: C)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): Expr[Value] =
    fn3[A,B,C](fnName).apply(a,b,c)

  final class Apply3[A,B,C](mkExpr: (String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): Expr[Value] =
      compile.apply(a,b,c)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): (A,B,C) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C]): (A,B,C) => Z = {
      val ps = Array[ExprParam[_]](A,B,C).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2)), post)
      (a,b,c) => z(Array[Any](a,b,c).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply4[A,B,C,D](mkExpr: (String,String,String,String) => String, a: A, b: B, c: C, d: D)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): Expr[Value] =
    apply4[A,B,C,D](mkExpr).apply(a,b,c,d)

  final def apply4[A,B,C,D](mkExpr: (String,String,String,String) => String): Apply4[A,B,C,D] =
    new Apply4(mkExpr)

  final def fn4[A,B,C,D](fnName: String): Apply4[A,B,C,D] =
    apply4((a,b,c,d) => s"$fnName($a,$b,$c,$d)")

  final def fn4[A,B,C,D](fnName: String, a: A, b: B, c: C, d: D)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): Expr[Value] =
    fn4[A,B,C,D](fnName).apply(a,b,c,d)

  final class Apply4[A,B,C,D](mkExpr: (String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): Expr[Value] =
      compile.apply(a,b,c,d)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): (A,B,C,D) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D]): (A,B,C,D) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3)), post)
      (a,b,c,d) => z(Array[Any](a,b,c,d).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): Expr[Value] =
    apply5[A,B,C,D,E](mkExpr).apply(a,b,c,d,e)

  final def apply5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String): Apply5[A,B,C,D,E] =
    new Apply5(mkExpr)

  final def fn5[A,B,C,D,E](fnName: String): Apply5[A,B,C,D,E] =
    apply5((a,b,c,d,e) => s"$fnName($a,$b,$c,$d,$e)")

  final def fn5[A,B,C,D,E](fnName: String, a: A, b: B, c: C, d: D, e: E)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): Expr[Value] =
    fn5[A,B,C,D,E](fnName).apply(a,b,c,d,e)

  final class Apply5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): Expr[Value] =
      compile.apply(a,b,c,d,e)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): (A,B,C,D,E) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E]): (A,B,C,D,E) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4)), post)
      (a,b,c,d,e) => z(Array[Any](a,b,c,d,e).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): Expr[Value] =
    apply6[A,B,C,D,E,F](mkExpr).apply(a,b,c,d,e,f)

  final def apply6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String): Apply6[A,B,C,D,E,F] =
    new Apply6(mkExpr)

  final def fn6[A,B,C,D,E,F](fnName: String): Apply6[A,B,C,D,E,F] =
    apply6((a,b,c,d,e,f) => s"$fnName($a,$b,$c,$d,$e,$f)")

  final def fn6[A,B,C,D,E,F](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): Expr[Value] =
    fn6[A,B,C,D,E,F](fnName).apply(a,b,c,d,e,f)

  final class Apply6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): Expr[Value] =
      compile.apply(a,b,c,d,e,f)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): (A,B,C,D,E,F) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F]): (A,B,C,D,E,F) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5)), post)
      (a,b,c,d,e,f) => z(Array[Any](a,b,c,d,e,f).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): Expr[Value] =
    apply7[A,B,C,D,E,F,G](mkExpr).apply(a,b,c,d,e,f,g)

  final def apply7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String): Apply7[A,B,C,D,E,F,G] =
    new Apply7(mkExpr)

  final def fn7[A,B,C,D,E,F,G](fnName: String): Apply7[A,B,C,D,E,F,G] =
    apply7((a,b,c,d,e,f,g) => s"$fnName($a,$b,$c,$d,$e,$f,$g)")

  final def fn7[A,B,C,D,E,F,G](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): Expr[Value] =
    fn7[A,B,C,D,E,F,G](fnName).apply(a,b,c,d,e,f,g)

  final class Apply7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): (A,B,C,D,E,F,G) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G]): (A,B,C,D,E,F,G) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6)), post)
      (a,b,c,d,e,f,g) => z(Array[Any](a,b,c,d,e,f,g).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): Expr[Value] =
    apply8[A,B,C,D,E,F,G,H](mkExpr).apply(a,b,c,d,e,f,g,h)

  final def apply8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String): Apply8[A,B,C,D,E,F,G,H] =
    new Apply8(mkExpr)

  final def fn8[A,B,C,D,E,F,G,H](fnName: String): Apply8[A,B,C,D,E,F,G,H] =
    apply8((a,b,c,d,e,f,g,h) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h)")

  final def fn8[A,B,C,D,E,F,G,H](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): Expr[Value] =
    fn8[A,B,C,D,E,F,G,H](fnName).apply(a,b,c,d,e,f,g,h)

  final class Apply8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): (A,B,C,D,E,F,G,H) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H]): (A,B,C,D,E,F,G,H) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7)), post)
      (a,b,c,d,e,f,g,h) => z(Array[Any](a,b,c,d,e,f,g,h).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): Expr[Value] =
    apply9[A,B,C,D,E,F,G,H,I](mkExpr).apply(a,b,c,d,e,f,g,h,i)

  final def apply9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String): Apply9[A,B,C,D,E,F,G,H,I] =
    new Apply9(mkExpr)

  final def fn9[A,B,C,D,E,F,G,H,I](fnName: String): Apply9[A,B,C,D,E,F,G,H,I] =
    apply9((a,b,c,d,e,f,g,h,i) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i)")

  final def fn9[A,B,C,D,E,F,G,H,I](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): Expr[Value] =
    fn9[A,B,C,D,E,F,G,H,I](fnName).apply(a,b,c,d,e,f,g,h,i)

  final class Apply9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): (A,B,C,D,E,F,G,H,I) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I]): (A,B,C,D,E,F,G,H,I) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8)), post)
      (a,b,c,d,e,f,g,h,i) => z(Array[Any](a,b,c,d,e,f,g,h,i).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): Expr[Value] =
    apply10[A,B,C,D,E,F,G,H,I,J](mkExpr).apply(a,b,c,d,e,f,g,h,i,j)

  final def apply10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String): Apply10[A,B,C,D,E,F,G,H,I,J] =
    new Apply10(mkExpr)

  final def fn10[A,B,C,D,E,F,G,H,I,J](fnName: String): Apply10[A,B,C,D,E,F,G,H,I,J] =
    apply10((a,b,c,d,e,f,g,h,i,j) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j)")

  final def fn10[A,B,C,D,E,F,G,H,I,J](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): Expr[Value] =
    fn10[A,B,C,D,E,F,G,H,I,J](fnName).apply(a,b,c,d,e,f,g,h,i,j)

  final class Apply10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): (A,B,C,D,E,F,G,H,I,J) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J]): (A,B,C,D,E,F,G,H,I,J) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9)), post)
      (a,b,c,d,e,f,g,h,i,j) => z(Array[Any](a,b,c,d,e,f,g,h,i,j).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): Expr[Value] =
    apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k)

  final def apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String): Apply11[A,B,C,D,E,F,G,H,I,J,K] =
    new Apply11(mkExpr)

  final def fn11[A,B,C,D,E,F,G,H,I,J,K](fnName: String): Apply11[A,B,C,D,E,F,G,H,I,J,K] =
    apply11((a,b,c,d,e,f,g,h,i,j,k) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k)")

  final def fn11[A,B,C,D,E,F,G,H,I,J,K](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): Expr[Value] =
    fn11[A,B,C,D,E,F,G,H,I,J,K](fnName).apply(a,b,c,d,e,f,g,h,i,j,k)

  final class Apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): (A,B,C,D,E,F,G,H,I,J,K) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K]): (A,B,C,D,E,F,G,H,I,J,K) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10)), post)
      (a,b,c,d,e,f,g,h,i,j,k) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): Expr[Value] =
    apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l)

  final def apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply12[A,B,C,D,E,F,G,H,I,J,K,L] =
    new Apply12(mkExpr)

  final def fn12[A,B,C,D,E,F,G,H,I,J,K,L](fnName: String): Apply12[A,B,C,D,E,F,G,H,I,J,K,L] =
    apply12((a,b,c,d,e,f,g,h,i,j,k,l) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l)")

  final def fn12[A,B,C,D,E,F,G,H,I,J,K,L](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): Expr[Value] =
    fn12[A,B,C,D,E,F,G,H,I,J,K,L](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l)

  final class Apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): Expr[Value] =
    apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m)

  final def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply13[A,B,C,D,E,F,G,H,I,J,K,L,M] =
    new Apply13(mkExpr)

  final def fn13[A,B,C,D,E,F,G,H,I,J,K,L,M](fnName: String): Apply13[A,B,C,D,E,F,G,H,I,J,K,L,M] =
    apply13((a,b,c,d,e,f,g,h,i,j,k,l,m) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m)")

  final def fn13[A,B,C,D,E,F,G,H,I,J,K,L,M](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): Expr[Value] =
    fn13[A,B,C,D,E,F,G,H,I,J,K,L,M](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m)

  final class Apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): Expr[Value] =
    apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

  final def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] =
    new Apply14(mkExpr)

  final def fn14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](fnName: String): Apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] =
    apply14((a,b,c,d,e,f,g,h,i,j,k,l,m,n) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n)")

  final def fn14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): Expr[Value] =
    fn14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

  final class Apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): Expr[Value] =
    apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

  final def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] =
    new Apply15(mkExpr)

  final def fn15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](fnName: String): Apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] =
    apply15((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o)")

  final def fn15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): Expr[Value] =
    fn15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

  final class Apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): Expr[Value] =
    apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

  final def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] =
    new Apply16(mkExpr)

  final def fn16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](fnName: String): Apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] =
    apply16((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p)")

  final def fn16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): Expr[Value] =
    fn16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

  final class Apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): Expr[Value] =
    apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

  final def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] =
    new Apply17(mkExpr)

  final def fn17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](fnName: String): Apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] =
    apply17((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q)")

  final def fn17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): Expr[Value] =
    fn17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

  final class Apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): Expr[Value] =
    apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

  final def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] =
    new Apply18(mkExpr)

  final def fn18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](fnName: String): Apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] =
    apply18((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r)")

  final def fn18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): Expr[Value] =
    fn18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

  final class Apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): Expr[Value] =
    apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

  final def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] =
    new Apply19(mkExpr)

  final def fn19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](fnName: String): Apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] =
    apply19((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s)")

  final def fn19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): Expr[Value] =
    fn19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

  final class Apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): Expr[Value] =
    apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

  final def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] =
    new Apply20(mkExpr)

  final def fn20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](fnName: String): Apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] =
    apply20((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t)")

  final def fn20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): Expr[Value] =
    fn20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

  final class Apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): Expr[Value] =
    apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

  final def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] =
    new Apply21(mkExpr)

  final def fn21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](fnName: String): Apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] =
    apply21((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u)")

  final def fn21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): Expr[Value] =
    fn21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

  final class Apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u).asInstanceOf[Array[X]])
    }
  }

  // ===================================================================================================================

  final def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): Expr[Value] =
    apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)

  final def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String): Apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] =
    new Apply22(mkExpr)

  final def fn22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fnName: String): Apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] =
    apply22((a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => s"$fnName($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v)")

  final def fn22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fnName: String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): Expr[Value] =
    fn22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fnName).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)

  final class Apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String) {

    @inline def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): Expr[Value] =
      compile.apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)

    def compile(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Expr[Value] =
      compile[Expr[Value]](exprValueId)

    def compile[Z](post: Expr[Value] => Z)(implicit lang: Language, A:ExprParam[A], B:ExprParam[B], C:ExprParam[C], D:ExprParam[D], E:ExprParam[E], F:ExprParam[F], G:ExprParam[G], H:ExprParam[H], I:ExprParam[I], J:ExprParam[J], K:ExprParam[K], L:ExprParam[L], M:ExprParam[M], N:ExprParam[N], O:ExprParam[O], P:ExprParam[P], Q:ExprParam[Q], R:ExprParam[R], S:ExprParam[S], T:ExprParam[T], U:ExprParam[U], V:ExprParam[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z = {
      val ps = Array[ExprParam[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).asInstanceOf[Array[ExprParam[X]]]
      val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20),e(21)), post)
      (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v).asInstanceOf[Array[X]])
    }
  }
}