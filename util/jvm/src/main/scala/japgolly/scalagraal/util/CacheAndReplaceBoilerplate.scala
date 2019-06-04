package japgolly.scalagraal.util

import cats.{Functor, Id}
import cats.syntax.functor._
import CacheAndReplace.Param

abstract class CacheAndReplaceBoilerplate private[util]() {

  protected def compileGeneric[F[_]: Functor](arity: Int, f: Array[String] => F[String]): F[Array[String] => String]

  // ===================================================================================================================

  def compileI1[A](f: (A) => String)(implicit A:Param[A]): (A) => String =
    compile1[Id, A](f)

  def compile1[Z[_], A](f: (A) => Z[String])(implicit Z: Functor[Z], A:Param[A]): Z[(A) => String] =
    compileGeneric(1, a => f(A.fromStr(a(0)))).map(x => (a: A) => x(Array(A.toStr(a))))

  // ===================================================================================================================

  def compileI2[A,B](f: (A,B) => String)(implicit A:Param[A], B:Param[B]): (A,B) => String =
    compile2[Id, A,B](f)

  def compile2[Z[_], A,B](f: (A,B) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B]): Z[(A,B) => String] =
    compileGeneric(2, a => f(A.fromStr(a(0)),B.fromStr(a(1)))).map(x => (a: A, b: B) => x(Array(A.toStr(a),B.toStr(b))))

  // ===================================================================================================================

  def compileI3[A,B,C](f: (A,B,C) => String)(implicit A:Param[A], B:Param[B], C:Param[C]): (A,B,C) => String =
    compile3[Id, A,B,C](f)

  def compile3[Z[_], A,B,C](f: (A,B,C) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C]): Z[(A,B,C) => String] =
    compileGeneric(3, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)))).map(x => (a: A, b: B, c: C) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c))))

  // ===================================================================================================================

  def compileI4[A,B,C,D](f: (A,B,C,D) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D]): (A,B,C,D) => String =
    compile4[Id, A,B,C,D](f)

  def compile4[Z[_], A,B,C,D](f: (A,B,C,D) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D]): Z[(A,B,C,D) => String] =
    compileGeneric(4, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)))).map(x => (a: A, b: B, c: C, d: D) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d))))

  // ===================================================================================================================

  def compileI5[A,B,C,D,E](f: (A,B,C,D,E) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): (A,B,C,D,E) => String =
    compile5[Id, A,B,C,D,E](f)

  def compile5[Z[_], A,B,C,D,E](f: (A,B,C,D,E) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): Z[(A,B,C,D,E) => String] =
    compileGeneric(5, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)))).map(x => (a: A, b: B, c: C, d: D, e: E) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e))))

  // ===================================================================================================================

  def compileI6[A,B,C,D,E,F](f: (A,B,C,D,E,F) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): (A,B,C,D,E,F) => String =
    compile6[Id, A,B,C,D,E,F](f)

  def compile6[Z[_], A,B,C,D,E,F](f: (A,B,C,D,E,F) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): Z[(A,B,C,D,E,F) => String] =
    compileGeneric(6, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f))))

  // ===================================================================================================================

  def compileI7[A,B,C,D,E,F,G](f: (A,B,C,D,E,F,G) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): (A,B,C,D,E,F,G) => String =
    compile7[Id, A,B,C,D,E,F,G](f)

  def compile7[Z[_], A,B,C,D,E,F,G](f: (A,B,C,D,E,F,G) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): Z[(A,B,C,D,E,F,G) => String] =
    compileGeneric(7, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g))))

  // ===================================================================================================================

  def compileI8[A,B,C,D,E,F,G,H](f: (A,B,C,D,E,F,G,H) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): (A,B,C,D,E,F,G,H) => String =
    compile8[Id, A,B,C,D,E,F,G,H](f)

  def compile8[Z[_], A,B,C,D,E,F,G,H](f: (A,B,C,D,E,F,G,H) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): Z[(A,B,C,D,E,F,G,H) => String] =
    compileGeneric(8, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h))))

  // ===================================================================================================================

  def compileI9[A,B,C,D,E,F,G,H,I](f: (A,B,C,D,E,F,G,H,I) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): (A,B,C,D,E,F,G,H,I) => String =
    compile9[Id, A,B,C,D,E,F,G,H,I](f)

  def compile9[Z[_], A,B,C,D,E,F,G,H,I](f: (A,B,C,D,E,F,G,H,I) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): Z[(A,B,C,D,E,F,G,H,I) => String] =
    compileGeneric(9, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i))))

  // ===================================================================================================================

  def compileI10[A,B,C,D,E,F,G,H,I,J](f: (A,B,C,D,E,F,G,H,I,J) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): (A,B,C,D,E,F,G,H,I,J) => String =
    compile10[Id, A,B,C,D,E,F,G,H,I,J](f)

  def compile10[Z[_], A,B,C,D,E,F,G,H,I,J](f: (A,B,C,D,E,F,G,H,I,J) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): Z[(A,B,C,D,E,F,G,H,I,J) => String] =
    compileGeneric(10, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j))))

  // ===================================================================================================================

  def compileI11[A,B,C,D,E,F,G,H,I,J,K](f: (A,B,C,D,E,F,G,H,I,J,K) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): (A,B,C,D,E,F,G,H,I,J,K) => String =
    compile11[Id, A,B,C,D,E,F,G,H,I,J,K](f)

  def compile11[Z[_], A,B,C,D,E,F,G,H,I,J,K](f: (A,B,C,D,E,F,G,H,I,J,K) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): Z[(A,B,C,D,E,F,G,H,I,J,K) => String] =
    compileGeneric(11, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k))))

  // ===================================================================================================================

  def compileI12[A,B,C,D,E,F,G,H,I,J,K,L](f: (A,B,C,D,E,F,G,H,I,J,K,L) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => String =
    compile12[Id, A,B,C,D,E,F,G,H,I,J,K,L](f)

  def compile12[Z[_], A,B,C,D,E,F,G,H,I,J,K,L](f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): Z[(A,B,C,D,E,F,G,H,I,J,K,L) => String] =
    compileGeneric(12, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l))))

  // ===================================================================================================================

  def compileI13[A,B,C,D,E,F,G,H,I,J,K,L,M](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => String =
    compile13[Id, A,B,C,D,E,F,G,H,I,J,K,L,M](f)

  def compile13[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M) => String] =
    compileGeneric(13, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m))))

  // ===================================================================================================================

  def compileI14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String =
    compile14[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N](f)

  def compile14[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String] =
    compileGeneric(14, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n))))

  // ===================================================================================================================

  def compileI15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String =
    compile15[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f)

  def compile15[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String] =
    compileGeneric(15, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o))))

  // ===================================================================================================================

  def compileI16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String =
    compile16[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f)

  def compile16[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String] =
    compileGeneric(16, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p))))

  // ===================================================================================================================

  def compileI17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String =
    compile17[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f)

  def compile17[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String] =
    compileGeneric(17, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q))))

  // ===================================================================================================================

  def compileI18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String =
    compile18[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f)

  def compile18[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String] =
    compileGeneric(18, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r))))

  // ===================================================================================================================

  def compileI19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String =
    compile19[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f)

  def compile19[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String] =
    compileGeneric(19, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s))))

  // ===================================================================================================================

  def compileI20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String =
    compile20[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f)

  def compile20[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String] =
    compileGeneric(20, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t))))

  // ===================================================================================================================

  def compileI21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String =
    compile21[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f)

  def compile21[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String] =
    compileGeneric(21, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)),U.fromStr(a(20)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t),U.toStr(u))))

  // ===================================================================================================================

  def compileI22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String =
    compile22[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f)

  def compile22[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z[String])(implicit Z: Functor[Z], A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String] =
    compileGeneric(22, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)),U.fromStr(a(20)),V.fromStr(a(21)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t),U.toStr(u),V.toStr(v))))
}