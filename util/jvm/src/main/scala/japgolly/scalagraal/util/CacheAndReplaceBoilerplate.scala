package japgolly.scalagraal.util

import cats.{Functor, Id}
import cats.syntax.functor._
import CacheAndReplace.Param

abstract class CacheAndReplaceBoilerplate private[util]() {

  protected def compileGeneric[F[_]: Functor](arity: Int, f: Array[String] => F[String]): F[Array[String] => String]

  // ===================================================================================================================

  def compile1[A](f: (A) => String)(implicit A:Param[A]): (A) => String =
    compileF1[Id, A](f)

  def compileF1[Z[_], A](f: (A) => Z[String])(implicit A:Param[A], Z: Functor[Z]): Z[(A) => String] =
    compileGeneric(1, a => f(A.fromStr(a(0)))).map(x => (a: A) => x(Array(A.toStr(a))))

  // ===================================================================================================================

  def compile2[A,B](f: (A,B) => String)(implicit A:Param[A], B:Param[B]): (A,B) => String =
    compileF2[Id, A,B](f)

  def compileF2[Z[_], A,B](f: (A,B) => Z[String])(implicit A:Param[A], B:Param[B], Z: Functor[Z]): Z[(A,B) => String] =
    compileGeneric(2, a => f(A.fromStr(a(0)),B.fromStr(a(1)))).map(x => (a: A, b: B) => x(Array(A.toStr(a),B.toStr(b))))

  // ===================================================================================================================

  def compile3[A,B,C](f: (A,B,C) => String)(implicit A:Param[A], B:Param[B], C:Param[C]): (A,B,C) => String =
    compileF3[Id, A,B,C](f)

  def compileF3[Z[_], A,B,C](f: (A,B,C) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], Z: Functor[Z]): Z[(A,B,C) => String] =
    compileGeneric(3, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)))).map(x => (a: A, b: B, c: C) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c))))

  // ===================================================================================================================

  def compile4[A,B,C,D](f: (A,B,C,D) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D]): (A,B,C,D) => String =
    compileF4[Id, A,B,C,D](f)

  def compileF4[Z[_], A,B,C,D](f: (A,B,C,D) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], Z: Functor[Z]): Z[(A,B,C,D) => String] =
    compileGeneric(4, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)))).map(x => (a: A, b: B, c: C, d: D) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d))))

  // ===================================================================================================================

  def compile5[A,B,C,D,E](f: (A,B,C,D,E) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E]): (A,B,C,D,E) => String =
    compileF5[Id, A,B,C,D,E](f)

  def compileF5[Z[_], A,B,C,D,E](f: (A,B,C,D,E) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], Z: Functor[Z]): Z[(A,B,C,D,E) => String] =
    compileGeneric(5, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)))).map(x => (a: A, b: B, c: C, d: D, e: E) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e))))

  // ===================================================================================================================

  def compile6[A,B,C,D,E,F](f: (A,B,C,D,E,F) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F]): (A,B,C,D,E,F) => String =
    compileF6[Id, A,B,C,D,E,F](f)

  def compileF6[Z[_], A,B,C,D,E,F](f: (A,B,C,D,E,F) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], Z: Functor[Z]): Z[(A,B,C,D,E,F) => String] =
    compileGeneric(6, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f))))

  // ===================================================================================================================

  def compile7[A,B,C,D,E,F,G](f: (A,B,C,D,E,F,G) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G]): (A,B,C,D,E,F,G) => String =
    compileF7[Id, A,B,C,D,E,F,G](f)

  def compileF7[Z[_], A,B,C,D,E,F,G](f: (A,B,C,D,E,F,G) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], Z: Functor[Z]): Z[(A,B,C,D,E,F,G) => String] =
    compileGeneric(7, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g))))

  // ===================================================================================================================

  def compile8[A,B,C,D,E,F,G,H](f: (A,B,C,D,E,F,G,H) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H]): (A,B,C,D,E,F,G,H) => String =
    compileF8[Id, A,B,C,D,E,F,G,H](f)

  def compileF8[Z[_], A,B,C,D,E,F,G,H](f: (A,B,C,D,E,F,G,H) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H) => String] =
    compileGeneric(8, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h))))

  // ===================================================================================================================

  def compile9[A,B,C,D,E,F,G,H,I](f: (A,B,C,D,E,F,G,H,I) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I]): (A,B,C,D,E,F,G,H,I) => String =
    compileF9[Id, A,B,C,D,E,F,G,H,I](f)

  def compileF9[Z[_], A,B,C,D,E,F,G,H,I](f: (A,B,C,D,E,F,G,H,I) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I) => String] =
    compileGeneric(9, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i))))

  // ===================================================================================================================

  def compile10[A,B,C,D,E,F,G,H,I,J](f: (A,B,C,D,E,F,G,H,I,J) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J]): (A,B,C,D,E,F,G,H,I,J) => String =
    compileF10[Id, A,B,C,D,E,F,G,H,I,J](f)

  def compileF10[Z[_], A,B,C,D,E,F,G,H,I,J](f: (A,B,C,D,E,F,G,H,I,J) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J) => String] =
    compileGeneric(10, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j))))

  // ===================================================================================================================

  def compile11[A,B,C,D,E,F,G,H,I,J,K](f: (A,B,C,D,E,F,G,H,I,J,K) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K]): (A,B,C,D,E,F,G,H,I,J,K) => String =
    compileF11[Id, A,B,C,D,E,F,G,H,I,J,K](f)

  def compileF11[Z[_], A,B,C,D,E,F,G,H,I,J,K](f: (A,B,C,D,E,F,G,H,I,J,K) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K) => String] =
    compileGeneric(11, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k))))

  // ===================================================================================================================

  def compile12[A,B,C,D,E,F,G,H,I,J,K,L](f: (A,B,C,D,E,F,G,H,I,J,K,L) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L]): (A,B,C,D,E,F,G,H,I,J,K,L) => String =
    compileF12[Id, A,B,C,D,E,F,G,H,I,J,K,L](f)

  def compileF12[Z[_], A,B,C,D,E,F,G,H,I,J,K,L](f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L) => String] =
    compileGeneric(12, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l))))

  // ===================================================================================================================

  def compile13[A,B,C,D,E,F,G,H,I,J,K,L,M](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M]): (A,B,C,D,E,F,G,H,I,J,K,L,M) => String =
    compileF13[Id, A,B,C,D,E,F,G,H,I,J,K,L,M](f)

  def compileF13[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M) => String] =
    compileGeneric(13, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m))))

  // ===================================================================================================================

  def compile14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String =
    compileF14[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N](f)

  def compileF14[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N) => String] =
    compileGeneric(14, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n))))

  // ===================================================================================================================

  def compile15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String =
    compileF15[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f)

  def compileF15[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => String] =
    compileGeneric(15, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o))))

  // ===================================================================================================================

  def compile16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String =
    compileF16[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f)

  def compileF16[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => String] =
    compileGeneric(16, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p))))

  // ===================================================================================================================

  def compile17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String =
    compileF17[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f)

  def compileF17[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => String] =
    compileGeneric(17, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q))))

  // ===================================================================================================================

  def compile18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String =
    compileF18[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f)

  def compileF18[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => String] =
    compileGeneric(18, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r))))

  // ===================================================================================================================

  def compile19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String =
    compileF19[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f)

  def compileF19[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => String] =
    compileGeneric(19, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s))))

  // ===================================================================================================================

  def compile20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String =
    compileF20[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f)

  def compileF20[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => String] =
    compileGeneric(20, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t))))

  // ===================================================================================================================

  def compile21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String =
    compileF21[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f)

  def compileF21[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => String] =
    compileGeneric(21, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)),U.fromStr(a(20)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t),U.toStr(u))))

  // ===================================================================================================================

  def compile22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String)(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V]): (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String =
    compileF22[Id, A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f)

  def compileF22[Z[_], A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z[String])(implicit A:Param[A], B:Param[B], C:Param[C], D:Param[D], E:Param[E], F:Param[F], G:Param[G], H:Param[H], I:Param[I], J:Param[J], K:Param[K], L:Param[L], M:Param[M], N:Param[N], O:Param[O], P:Param[P], Q:Param[Q], R:Param[R], S:Param[S], T:Param[T], U:Param[U], V:Param[V], Z: Functor[Z]): Z[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => String] =
    compileGeneric(22, a => f(A.fromStr(a(0)),B.fromStr(a(1)),C.fromStr(a(2)),D.fromStr(a(3)),E.fromStr(a(4)),F.fromStr(a(5)),G.fromStr(a(6)),H.fromStr(a(7)),I.fromStr(a(8)),J.fromStr(a(9)),K.fromStr(a(10)),L.fromStr(a(11)),M.fromStr(a(12)),N.fromStr(a(13)),O.fromStr(a(14)),P.fromStr(a(15)),Q.fromStr(a(16)),R.fromStr(a(17)),S.fromStr(a(18)),T.fromStr(a(19)),U.fromStr(a(20)),V.fromStr(a(21)))).map(x => (a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) => x(Array(A.toStr(a),B.toStr(b),C.toStr(c),D.toStr(d),E.toStr(e),F.toStr(f),G.toStr(g),H.toStr(h),I.toStr(i),J.toStr(j),K.toStr(k),L.toStr(l),M.toStr(m),N.toStr(n),O.toStr(o),P.toStr(p),Q.toStr(q),R.toStr(r),S.toStr(s),T.toStr(t),U.toStr(u),V.toStr(v))))
}