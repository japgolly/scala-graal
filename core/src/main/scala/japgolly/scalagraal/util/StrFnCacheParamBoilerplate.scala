package japgolly.scalagraal.util

import japgolly.scalagraal.util.StrFnCachePath._
import scala.reflect.ClassTag

abstract class StrFnCacheParamBoilerplate private[util]() {

  def apply[A](paths: List[StrFnCachePath[A]]): StrFnCacheParam[A]


  // ===================================================================================================================

  final def apply2[A,B,Z](f: (A,B) => Z)(g: Z => (A,B))(implicit t: StrFnCacheParam[(A,B)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2))(g)

  final def divide2[A<:Z:ClassTag, B<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple2[A,B](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B]): StrFnCacheParam[(A,B)] = {
    type Z = (A,B)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tokens = (tA.tokens, tB.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply3[A,B,C,Z](f: (A,B,C) => Z)(g: Z => (A,B,C))(implicit t: StrFnCacheParam[(A,B,C)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3))(g)

  final def divide3[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple3[A,B,C](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C]): StrFnCacheParam[(A,B,C)] = {
    type Z = (A,B,C)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply4[A,B,C,D,Z](f: (A,B,C,D) => Z)(g: Z => (A,B,C,D))(implicit t: StrFnCacheParam[(A,B,C,D)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4))(g)

  final def divide4[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple4[A,B,C,D](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D]): StrFnCacheParam[(A,B,C,D)] = {
    type Z = (A,B,C,D)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply5[A,B,C,D,E,Z](f: (A,B,C,D,E) => Z)(g: Z => (A,B,C,D,E))(implicit t: StrFnCacheParam[(A,B,C,D,E)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5))(g)

  final def divide5[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple5[A,B,C,D,E](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E]): StrFnCacheParam[(A,B,C,D,E)] = {
    type Z = (A,B,C,D,E)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply6[A,B,C,D,E,F,Z](f: (A,B,C,D,E,F) => Z)(g: Z => (A,B,C,D,E,F))(implicit t: StrFnCacheParam[(A,B,C,D,E,F)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6))(g)

  final def divide6[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple6[A,B,C,D,E,F](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F]): StrFnCacheParam[(A,B,C,D,E,F)] = {
    type Z = (A,B,C,D,E,F)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply7[A,B,C,D,E,F,G,Z](f: (A,B,C,D,E,F,G) => Z)(g: Z => (A,B,C,D,E,F,G))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7))(g)

  final def divide7[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple7[A,B,C,D,E,F,G](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G]): StrFnCacheParam[(A,B,C,D,E,F,G)] = {
    type Z = (A,B,C,D,E,F,G)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply8[A,B,C,D,E,F,G,H,Z](f: (A,B,C,D,E,F,G,H) => Z)(g: Z => (A,B,C,D,E,F,G,H))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8))(g)

  final def divide8[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple8[A,B,C,D,E,F,G,H](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H]): StrFnCacheParam[(A,B,C,D,E,F,G,H)] = {
    type Z = (A,B,C,D,E,F,G,H)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply9[A,B,C,D,E,F,G,H,I,Z](f: (A,B,C,D,E,F,G,H,I) => Z)(g: Z => (A,B,C,D,E,F,G,H,I))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9))(g)

  final def divide9[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple9[A,B,C,D,E,F,G,H,I](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I)] = {
    type Z = (A,B,C,D,E,F,G,H,I)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply10[A,B,C,D,E,F,G,H,I,J,Z](f: (A,B,C,D,E,F,G,H,I,J) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10))(g)

  final def divide10[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple10[A,B,C,D,E,F,G,H,I,J](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply11[A,B,C,D,E,F,G,H,I,J,K,Z](f: (A,B,C,D,E,F,G,H,I,J,K) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11))(g)

  final def divide11[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple11[A,B,C,D,E,F,G,H,I,J,K](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply12[A,B,C,D,E,F,G,H,I,J,K,L,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12))(g)

  final def divide12[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple12[A,B,C,D,E,F,G,H,I,J,K,L](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13))(g)

  final def divide13[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14))(g)

  final def divide14[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15))(g)

  final def divide15[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16))(g)

  final def divide16[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17))(g)

  final def divide17[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18))(g)

  final def divide18[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, R<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z]) ++
      cR.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
        pR <- cR.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17) && pR.isApplicable(t._18),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tR = pR.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens, tR.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            tR.replacements.foreach(r => replacements ::= r.contramap[Z](_._18))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19))(g)

  final def divide19[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, R<:Z:ClassTag, S<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z]) ++
      cR.paths.iterator.map(_.widen[Z]) ++
      cS.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
        pR <- cR.paths
        pS <- cS.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17) && pR.isApplicable(t._18) && pS.isApplicable(t._19),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tR = pR.newTokens()
            val tS = pS.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens, tR.tokens, tS.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            tR.replacements.foreach(r => replacements ::= r.contramap[Z](_._18))
            tS.replacements.foreach(r => replacements ::= r.contramap[Z](_._19))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20))(g)

  final def divide20[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, R<:Z:ClassTag, S<:Z:ClassTag, T<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z]) ++
      cR.paths.iterator.map(_.widen[Z]) ++
      cS.paths.iterator.map(_.widen[Z]) ++
      cT.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
        pR <- cR.paths
        pS <- cS.paths
        pT <- cT.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17) && pR.isApplicable(t._18) && pS.isApplicable(t._19) && pT.isApplicable(t._20),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tR = pR.newTokens()
            val tS = pS.newTokens()
            val tT = pT.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens, tR.tokens, tS.tokens, tT.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            tR.replacements.foreach(r => replacements ::= r.contramap[Z](_._18))
            tS.replacements.foreach(r => replacements ::= r.contramap[Z](_._19))
            tT.replacements.foreach(r => replacements ::= r.contramap[Z](_._20))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21))(g)

  final def divide21[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, R<:Z:ClassTag, S<:Z:ClassTag, T<:Z:ClassTag, U<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T], cU: StrFnCacheParam[U]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z]) ++
      cR.paths.iterator.map(_.widen[Z]) ++
      cS.paths.iterator.map(_.widen[Z]) ++
      cT.paths.iterator.map(_.widen[Z]) ++
      cU.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T], cU: StrFnCacheParam[U]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
        pR <- cR.paths
        pS <- cS.paths
        pT <- cT.paths
        pU <- cU.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17) && pR.isApplicable(t._18) && pS.isApplicable(t._19) && pT.isApplicable(t._20) && pU.isApplicable(t._21),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tR = pR.newTokens()
            val tS = pS.newTokens()
            val tT = pT.newTokens()
            val tU = pU.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens, tR.tokens, tS.tokens, tT.tokens, tU.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            tR.replacements.foreach(r => replacements ::= r.contramap[Z](_._18))
            tS.replacements.foreach(r => replacements ::= r.contramap[Z](_._19))
            tT.replacements.foreach(r => replacements ::= r.contramap[Z](_._20))
            tU.replacements.foreach(r => replacements ::= r.contramap[Z](_._21))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }

  // ===================================================================================================================

  final def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V))(implicit t: StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)]): StrFnCacheParam[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21, x._22))(g)

  final def divide22[A<:Z:ClassTag, B<:Z:ClassTag, C<:Z:ClassTag, D<:Z:ClassTag, E<:Z:ClassTag, F<:Z:ClassTag, G<:Z:ClassTag, H<:Z:ClassTag, I<:Z:ClassTag, J<:Z:ClassTag, K<:Z:ClassTag, L<:Z:ClassTag, M<:Z:ClassTag, N<:Z:ClassTag, O<:Z:ClassTag, P<:Z:ClassTag, Q<:Z:ClassTag, R<:Z:ClassTag, S<:Z:ClassTag, T<:Z:ClassTag, U<:Z:ClassTag, V<:Z:ClassTag, Z](cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T], cU: StrFnCacheParam[U], cV: StrFnCacheParam[V]): StrFnCacheParam[Z] =
    apply((
      cA.paths.iterator.map(_.widen[Z]) ++
      cB.paths.iterator.map(_.widen[Z]) ++
      cC.paths.iterator.map(_.widen[Z]) ++
      cD.paths.iterator.map(_.widen[Z]) ++
      cE.paths.iterator.map(_.widen[Z]) ++
      cF.paths.iterator.map(_.widen[Z]) ++
      cG.paths.iterator.map(_.widen[Z]) ++
      cH.paths.iterator.map(_.widen[Z]) ++
      cI.paths.iterator.map(_.widen[Z]) ++
      cJ.paths.iterator.map(_.widen[Z]) ++
      cK.paths.iterator.map(_.widen[Z]) ++
      cL.paths.iterator.map(_.widen[Z]) ++
      cM.paths.iterator.map(_.widen[Z]) ++
      cN.paths.iterator.map(_.widen[Z]) ++
      cO.paths.iterator.map(_.widen[Z]) ++
      cP.paths.iterator.map(_.widen[Z]) ++
      cQ.paths.iterator.map(_.widen[Z]) ++
      cR.paths.iterator.map(_.widen[Z]) ++
      cS.paths.iterator.map(_.widen[Z]) ++
      cT.paths.iterator.map(_.widen[Z]) ++
      cU.paths.iterator.map(_.widen[Z]) ++
      cV.paths.iterator.map(_.widen[Z])
    ).toList)

  final implicit def tuple22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](implicit cA: StrFnCacheParam[A], cB: StrFnCacheParam[B], cC: StrFnCacheParam[C], cD: StrFnCacheParam[D], cE: StrFnCacheParam[E], cF: StrFnCacheParam[F], cG: StrFnCacheParam[G], cH: StrFnCacheParam[H], cI: StrFnCacheParam[I], cJ: StrFnCacheParam[J], cK: StrFnCacheParam[K], cL: StrFnCacheParam[L], cM: StrFnCacheParam[M], cN: StrFnCacheParam[N], cO: StrFnCacheParam[O], cP: StrFnCacheParam[P], cQ: StrFnCacheParam[Q], cR: StrFnCacheParam[R], cS: StrFnCacheParam[S], cT: StrFnCacheParam[T], cU: StrFnCacheParam[U], cV: StrFnCacheParam[V]): StrFnCacheParam[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] = {
    type Z = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
        pF <- cF.paths
        pG <- cG.paths
        pH <- cH.paths
        pI <- cI.paths
        pJ <- cJ.paths
        pK <- cK.paths
        pL <- cL.paths
        pM <- cM.paths
        pN <- cN.paths
        pO <- cO.paths
        pP <- cP.paths
        pQ <- cQ.paths
        pR <- cR.paths
        pS <- cS.paths
        pT <- cT.paths
        pU <- cU.paths
        pV <- cV.paths
      } yield
        StrFnCachePath[Z](
          t => pA.isApplicable(t._1) && pB.isApplicable(t._2) && pC.isApplicable(t._3) && pD.isApplicable(t._4) && pE.isApplicable(t._5) && pF.isApplicable(t._6) && pG.isApplicable(t._7) && pH.isApplicable(t._8) && pI.isApplicable(t._9) && pJ.isApplicable(t._10) && pK.isApplicable(t._11) && pL.isApplicable(t._12) && pM.isApplicable(t._13) && pN.isApplicable(t._14) && pO.isApplicable(t._15) && pP.isApplicable(t._16) && pQ.isApplicable(t._17) && pR.isApplicable(t._18) && pS.isApplicable(t._19) && pT.isApplicable(t._20) && pU.isApplicable(t._21) && pV.isApplicable(t._22),
          () => {
            val tA = pA.newTokens()
            val tB = pB.newTokens()
            val tC = pC.newTokens()
            val tD = pD.newTokens()
            val tE = pE.newTokens()
            val tF = pF.newTokens()
            val tG = pG.newTokens()
            val tH = pH.newTokens()
            val tI = pI.newTokens()
            val tJ = pJ.newTokens()
            val tK = pK.newTokens()
            val tL = pL.newTokens()
            val tM = pM.newTokens()
            val tN = pN.newTokens()
            val tO = pO.newTokens()
            val tP = pP.newTokens()
            val tQ = pQ.newTokens()
            val tR = pR.newTokens()
            val tS = pS.newTokens()
            val tT = pT.newTokens()
            val tU = pU.newTokens()
            val tV = pV.newTokens()
            val tokens = (tA.tokens, tB.tokens, tC.tokens, tD.tokens, tE.tokens, tF.tokens, tG.tokens, tH.tokens, tI.tokens, tJ.tokens, tK.tokens, tL.tokens, tM.tokens, tN.tokens, tO.tokens, tP.tokens, tQ.tokens, tR.tokens, tS.tokens, tT.tokens, tU.tokens, tV.tokens)
            var replacements = List.empty[Replacement[Z]]
            tA.replacements.foreach(r => replacements ::= r.contramap[Z](_._1))
            tB.replacements.foreach(r => replacements ::= r.contramap[Z](_._2))
            tC.replacements.foreach(r => replacements ::= r.contramap[Z](_._3))
            tD.replacements.foreach(r => replacements ::= r.contramap[Z](_._4))
            tE.replacements.foreach(r => replacements ::= r.contramap[Z](_._5))
            tF.replacements.foreach(r => replacements ::= r.contramap[Z](_._6))
            tG.replacements.foreach(r => replacements ::= r.contramap[Z](_._7))
            tH.replacements.foreach(r => replacements ::= r.contramap[Z](_._8))
            tI.replacements.foreach(r => replacements ::= r.contramap[Z](_._9))
            tJ.replacements.foreach(r => replacements ::= r.contramap[Z](_._10))
            tK.replacements.foreach(r => replacements ::= r.contramap[Z](_._11))
            tL.replacements.foreach(r => replacements ::= r.contramap[Z](_._12))
            tM.replacements.foreach(r => replacements ::= r.contramap[Z](_._13))
            tN.replacements.foreach(r => replacements ::= r.contramap[Z](_._14))
            tO.replacements.foreach(r => replacements ::= r.contramap[Z](_._15))
            tP.replacements.foreach(r => replacements ::= r.contramap[Z](_._16))
            tQ.replacements.foreach(r => replacements ::= r.contramap[Z](_._17))
            tR.replacements.foreach(r => replacements ::= r.contramap[Z](_._18))
            tS.replacements.foreach(r => replacements ::= r.contramap[Z](_._19))
            tT.replacements.foreach(r => replacements ::= r.contramap[Z](_._20))
            tU.replacements.foreach(r => replacements ::= r.contramap[Z](_._21))
            tV.replacements.foreach(r => replacements ::= r.contramap[Z](_._22))
            Tokens(tokens, replacements)
          }
        )
    apply(paths)
  }
}