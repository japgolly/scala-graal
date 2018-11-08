package japgolly.scalagraal

import org.graalvm.polyglot.Value
import Expr.Param

abstract class ExprBoilerplate private[scalagraal]() {

  protected final type X = AnyRef { type A = Unit }
  private[this] final val exprValueId = (a: Expr[Value]) => a

  protected def genericOpt[Z](params: Array[Param[X]],
                              mkExprStr: Array[String] => String,
                              post: Expr[Value] => Z)
                             (implicit lang: Language): Array[X] => Z

  final def compile1[A,Z](mkExpr: (String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A]): (A) => Z = {
    val ps = Array[Param[_]](A).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0)), post)
  (a) => z(Array[Any](a).asInstanceOf[Array[X]])
  }

  final def compile1[A](mkExpr: (String) => String)(implicit lang: Language, A:Param[A]): (A) => Expr[Value] =
    compile1[A,Expr[Value]](mkExpr, exprValueId)

  final def apply1[A](mkExpr: (String) => String, a: A)(implicit lang: Language, A:Param[A]): Expr[Value] =
    compile1[A](mkExpr).apply(a)

  // ===================================================================================================================

  final def compile2[A,B,Z](mkExpr: (String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B]): (A,B) => Z = {
    val ps = Array[Param[_]](A,B).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1)), post)
  (a,b) => z(Array[Any](a,b).asInstanceOf[Array[X]])
  }

  final def compile2[A,B](mkExpr: (String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B]): (A,B) => Expr[Value] =
    compile2[A,B,Expr[Value]](mkExpr, exprValueId)

  final def apply2[A,B](mkExpr: (String,String) => String, a: A, b: B)(implicit lang: Language, A:Param[A], B:Param[B]): Expr[Value] =
    compile2[A,B](mkExpr).apply(a,b)

  // ===================================================================================================================

  final def compile3[A,B,C,Z](mkExpr: (String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C]): (A,B,C) => Z = {
    val ps = Array[Param[_]](A,B,C).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2)), post)
  (a,b,c) => z(Array[Any](a,b,c).asInstanceOf[Array[X]])
  }

  final def compile3[A,B,C](mkExpr: (String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C]): (A,B,C) => Expr[Value] =
    compile3[A,B,C,Expr[Value]](mkExpr, exprValueId)

  final def apply3[A,B,C](mkExpr: (String,String,String) => String, a: A, b: B, c: C)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C]): Expr[Value] =
    compile3[A,B,C](mkExpr).apply(a,b,c)

  // ===================================================================================================================

  final def compile4[A,B,C,D,Z](mkExpr: (String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D]): (A,B,C,D) => Z = {
    val ps = Array[Param[_]](A,B,C,D).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3)), post)
  (a,b,c,d) => z(Array[Any](a,b,c,d).asInstanceOf[Array[X]])
  }

  final def compile4[A,B,C,D](mkExpr: (String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D]): (A,B,C,D) => Expr[Value] =
    compile4[A,B,C,D,Expr[Value]](mkExpr, exprValueId)

  final def apply4[A,B,C,D](mkExpr: (String,String,String,String) => String, a: A, b: B, c: C, d: D)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D]): Expr[Value] =
    compile4[A,B,C,D](mkExpr).apply(a,b,c,d)

  // ===================================================================================================================

  final def compile5[A,B,C,D,E,Z](mkExpr: (String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): (A,B,C,D,E) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4)), post)
  (a,b,c,d,e) => z(Array[Any](a,b,c,d,e).asInstanceOf[Array[X]])
  }

  final def compile5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): (A,B,C,D,E) => Expr[Value] =
    compile5[A,B,C,D,E,Expr[Value]](mkExpr, exprValueId)

  final def apply5[A,B,C,D,E](mkExpr: (String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): Expr[Value] =
    compile5[A,B,C,D,E](mkExpr).apply(a,b,c,d,e)

  // ===================================================================================================================

  final def compile6[A,B,C,D,E,F,Z](mkExpr: (String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): (A,B,C,D,E,F) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5)), post)
  (a,b,c,d,e,f) => z(Array[Any](a,b,c,d,e,f).asInstanceOf[Array[X]])
  }

  final def compile6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): (A,B,C,D,E,F) => Expr[Value] =
    compile6[A,B,C,D,E,F,Expr[Value]](mkExpr, exprValueId)

  final def apply6[A,B,C,D,E,F](mkExpr: (String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): Expr[Value] =
    compile6[A,B,C,D,E,F](mkExpr).apply(a,b,c,d,e,f)

  // ===================================================================================================================

  final def compile7[A,B,C,D,E,F,G,Z](mkExpr: (String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): (A,B,C,D,E,F,G) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6)), post)
  (a,b,c,d,e,f,g) => z(Array[Any](a,b,c,d,e,f,g).asInstanceOf[Array[X]])
  }

  final def compile7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): (A,B,C,D,E,F,G) => Expr[Value] =
    compile7[A,B,C,D,E,F,G,Expr[Value]](mkExpr, exprValueId)

  final def apply7[A,B,C,D,E,F,G](mkExpr: (String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): Expr[Value] =
    compile7[A,B,C,D,E,F,G](mkExpr).apply(a,b,c,d,e,f,g)

  // ===================================================================================================================

  final def compile8[A,B,C,D,E,F,G,H,Z](mkExpr: (String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): (A,B,C,D,E,F,G,H) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7)), post)
  (a,b,c,d,e,f,g,h) => z(Array[Any](a,b,c,d,e,f,g,h).asInstanceOf[Array[X]])
  }

  final def compile8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): (A,B,C,D,E,F,G,H) => Expr[Value] =
    compile8[A,B,C,D,E,F,G,H,Expr[Value]](mkExpr, exprValueId)

  final def apply8[A,B,C,D,E,F,G,H](mkExpr: (String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): Expr[Value] =
    compile8[A,B,C,D,E,F,G,H](mkExpr).apply(a,b,c,d,e,f,g,h)

  // ===================================================================================================================

  final def compile9[A,B,C,D,E,F,G,H,I,Z](mkExpr: (String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): (A,B,C,D,E,F,G,H,I) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8)), post)
  (a,b,c,d,e,f,g,h,i) => z(Array[Any](a,b,c,d,e,f,g,h,i).asInstanceOf[Array[X]])
  }

  final def compile9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): (A,B,C,D,E,F,G,H,I) => Expr[Value] =
    compile9[A,B,C,D,E,F,G,H,I,Expr[Value]](mkExpr, exprValueId)

  final def apply9[A,B,C,D,E,F,G,H,I](mkExpr: (String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): Expr[Value] =
    compile9[A,B,C,D,E,F,G,H,I](mkExpr).apply(a,b,c,d,e,f,g,h,i)

  // ===================================================================================================================

  final def compile10[A,B,C,D,E,F,G,H,I,J,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): (A,B,C,D,E,F,G,H,I,J) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9)), post)
  (a,b,c,d,e,f,g,h,i,j) => z(Array[Any](a,b,c,d,e,f,g,h,i,j).asInstanceOf[Array[X]])
  }

  final def compile10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): (A,B,C,D,E,F,G,H,I,J) => Expr[Value] =
    compile10[A,B,C,D,E,F,G,H,I,J,Expr[Value]](mkExpr, exprValueId)

  final def apply10[A,B,C,D,E,F,G,H,I,J](mkExpr: (String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): Expr[Value] =
    compile10[A,B,C,D,E,F,G,H,I,J](mkExpr).apply(a,b,c,d,e,f,g,h,i,j)

  // ===================================================================================================================

  final def compile11[A,B,C,D,E,F,G,H,I,J,K,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): (A,B,C,D,E,F,G,H,I,J,K) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10)), post)
  (a,b,c,d,e,f,g,h,i,j,k) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k).asInstanceOf[Array[X]])
  }

  final def compile11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): (A,B,C,D,E,F,G,H,I,J,K) => Expr[Value] =
    compile11[A,B,C,D,E,F,G,H,I,J,K,Expr[Value]](mkExpr, exprValueId)

  final def apply11[A,B,C,D,E,F,G,H,I,J,K](mkExpr: (String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): Expr[Value] =
    compile11[A,B,C,D,E,F,G,H,I,J,K](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k)

  // ===================================================================================================================

  final def compile12[A,B,C,D,E,F,G,H,I,J,K,L,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l).asInstanceOf[Array[X]])
  }

  final def compile12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => Expr[Value] =
    compile12[A,B,C,D,E,F,G,H,I,J,K,L,Expr[Value]](mkExpr, exprValueId)

  final def apply12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): Expr[Value] =
    compile12[A,B,C,D,E,F,G,H,I,J,K,L](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l)

  // ===================================================================================================================

  final def compile13[A,B,C,D,E,F,G,H,I,J,K,L,M,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m).asInstanceOf[Array[X]])
  }

  final def compile13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => Expr[Value] =
    compile13[A,B,C,D,E,F,G,H,I,J,K,L,M,Expr[Value]](mkExpr, exprValueId)

  final def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): Expr[Value] =
    compile13[A,B,C,D,E,F,G,H,I,J,K,L,M](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m)

  // ===================================================================================================================

  final def compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n).asInstanceOf[Array[X]])
  }

  final def compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Expr[Value] =
    compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Expr[Value]](mkExpr, exprValueId)

  final def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): Expr[Value] =
    compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

  // ===================================================================================================================

  final def compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o).asInstanceOf[Array[X]])
  }

  final def compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Expr[Value] =
    compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Expr[Value]](mkExpr, exprValueId)

  final def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): Expr[Value] =
    compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

  // ===================================================================================================================

  final def compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p).asInstanceOf[Array[X]])
  }

  final def compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Expr[Value] =
    compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Expr[Value]](mkExpr, exprValueId)

  final def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): Expr[Value] =
    compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)

  // ===================================================================================================================

  final def compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q).asInstanceOf[Array[X]])
  }

  final def compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Expr[Value] =
    compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Expr[Value]](mkExpr, exprValueId)

  final def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): Expr[Value] =
    compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

  // ===================================================================================================================

  final def compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r).asInstanceOf[Array[X]])
  }

  final def compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Expr[Value] =
    compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Expr[Value]](mkExpr, exprValueId)

  final def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): Expr[Value] =
    compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

  // ===================================================================================================================

  final def compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s).asInstanceOf[Array[X]])
  }

  final def compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Expr[Value] =
    compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Expr[Value]](mkExpr, exprValueId)

  final def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): Expr[Value] =
    compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s)

  // ===================================================================================================================

  final def compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t).asInstanceOf[Array[X]])
  }

  final def compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Expr[Value] =
    compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Expr[Value]](mkExpr, exprValueId)

  final def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): Expr[Value] =
    compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)

  // ===================================================================================================================

  final def compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u).asInstanceOf[Array[X]])
  }

  final def compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Expr[Value] =
    compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Expr[Value]](mkExpr, exprValueId)

  final def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): Expr[Value] =
    compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u)

  // ===================================================================================================================

  final def compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Z](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, post: Expr[Value] => Z)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z = {
    val ps = Array[Param[_]](A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V).asInstanceOf[Array[Param[X]]]
    val z = genericOpt(ps, e => mkExpr(e(0),e(1),e(2),e(3),e(4),e(5),e(6),e(7),e(8),e(9),e(10),e(11),e(12),e(13),e(14),e(15),e(16),e(17),e(18),e(19),e(20),e(21)), post)
  (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) => z(Array[Any](a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v).asInstanceOf[Array[X]])
  }

  final def compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Expr[Value] =
    compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Expr[Value]](mkExpr, exprValueId)

  final def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr: (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String) => String, a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V)(implicit lang: Language, A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): Expr[Value] =
    compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](mkExpr).apply(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v)
}