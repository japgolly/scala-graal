package japgolly.scalagraal.util

import japgolly.scalagraal.util.StringGenCachePath._

abstract class StringGenCacheBoilerplate private[util]() {

  def apply[A](paths: List[StringGenCachePath[A]]): StringGenCache[A]


  // ===================================================================================================================

  def apply2[A,B,Z](f: (A,B) => Z)(g: Z => (A,B))(implicit t: StringGenCache[(A,B)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2))(g)

  implicit def tuple2[A,B](cA: StringGenCache[A], cB: StringGenCache[B]): StringGenCache[(A,B)] = {
    type Z = (A,B)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
      } yield
        StringGenCachePath[Z](
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

  def apply3[A,B,C,Z](f: (A,B,C) => Z)(g: Z => (A,B,C))(implicit t: StringGenCache[(A,B,C)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3))(g)

  implicit def tuple3[A,B,C](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C]): StringGenCache[(A,B,C)] = {
    type Z = (A,B,C)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
      } yield
        StringGenCachePath[Z](
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

  def apply4[A,B,C,D,Z](f: (A,B,C,D) => Z)(g: Z => (A,B,C,D))(implicit t: StringGenCache[(A,B,C,D)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4))(g)

  implicit def tuple4[A,B,C,D](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D]): StringGenCache[(A,B,C,D)] = {
    type Z = (A,B,C,D)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
      } yield
        StringGenCachePath[Z](
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

  def apply5[A,B,C,D,E,Z](f: (A,B,C,D,E) => Z)(g: Z => (A,B,C,D,E))(implicit t: StringGenCache[(A,B,C,D,E)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5))(g)

  implicit def tuple5[A,B,C,D,E](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E]): StringGenCache[(A,B,C,D,E)] = {
    type Z = (A,B,C,D,E)
    val paths =
      for {
        pA <- cA.paths
        pB <- cB.paths
        pC <- cC.paths
        pD <- cD.paths
        pE <- cE.paths
      } yield
        StringGenCachePath[Z](
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

  def apply6[A,B,C,D,E,F,Z](f: (A,B,C,D,E,F) => Z)(g: Z => (A,B,C,D,E,F))(implicit t: StringGenCache[(A,B,C,D,E,F)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6))(g)

  implicit def tuple6[A,B,C,D,E,F](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F]): StringGenCache[(A,B,C,D,E,F)] = {
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
        StringGenCachePath[Z](
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

  def apply7[A,B,C,D,E,F,G,Z](f: (A,B,C,D,E,F,G) => Z)(g: Z => (A,B,C,D,E,F,G))(implicit t: StringGenCache[(A,B,C,D,E,F,G)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7))(g)

  implicit def tuple7[A,B,C,D,E,F,G](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G]): StringGenCache[(A,B,C,D,E,F,G)] = {
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
        StringGenCachePath[Z](
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

  def apply8[A,B,C,D,E,F,G,H,Z](f: (A,B,C,D,E,F,G,H) => Z)(g: Z => (A,B,C,D,E,F,G,H))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8))(g)

  implicit def tuple8[A,B,C,D,E,F,G,H](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H]): StringGenCache[(A,B,C,D,E,F,G,H)] = {
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
        StringGenCachePath[Z](
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

  def apply9[A,B,C,D,E,F,G,H,I,Z](f: (A,B,C,D,E,F,G,H,I) => Z)(g: Z => (A,B,C,D,E,F,G,H,I))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9))(g)

  implicit def tuple9[A,B,C,D,E,F,G,H,I](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I]): StringGenCache[(A,B,C,D,E,F,G,H,I)] = {
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
        StringGenCachePath[Z](
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

  def apply10[A,B,C,D,E,F,G,H,I,J,Z](f: (A,B,C,D,E,F,G,H,I,J) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10))(g)

  implicit def tuple10[A,B,C,D,E,F,G,H,I,J](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J]): StringGenCache[(A,B,C,D,E,F,G,H,I,J)] = {
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
        StringGenCachePath[Z](
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

  def apply11[A,B,C,D,E,F,G,H,I,J,K,Z](f: (A,B,C,D,E,F,G,H,I,J,K) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11))(g)

  implicit def tuple11[A,B,C,D,E,F,G,H,I,J,K](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K)] = {
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
        StringGenCachePath[Z](
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

  def apply12[A,B,C,D,E,F,G,H,I,J,K,L,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12))(g)

  implicit def tuple12[A,B,C,D,E,F,G,H,I,J,K,L](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L)] = {
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
        StringGenCachePath[Z](
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

  def apply13[A,B,C,D,E,F,G,H,I,J,K,L,M,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13))(g)

  implicit def tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M)] = {
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
        StringGenCachePath[Z](
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

  def apply14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14))(g)

  implicit def tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)] = {
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
        StringGenCachePath[Z](
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

  def apply15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15))(g)

  implicit def tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)] = {
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
        StringGenCachePath[Z](
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

  def apply16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16))(g)

  implicit def tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)] = {
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
        StringGenCachePath[Z](
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

  def apply17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17))(g)

  implicit def tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)] = {
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
        StringGenCachePath[Z](
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

  def apply18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18))(g)

  implicit def tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q], cR: StringGenCache[R]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)] = {
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
        StringGenCachePath[Z](
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

  def apply19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19))(g)

  implicit def tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q], cR: StringGenCache[R], cS: StringGenCache[S]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)] = {
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
        StringGenCachePath[Z](
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

  def apply20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20))(g)

  implicit def tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q], cR: StringGenCache[R], cS: StringGenCache[S], cT: StringGenCache[T]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)] = {
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
        StringGenCachePath[Z](
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

  def apply21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21))(g)

  implicit def tuple21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q], cR: StringGenCache[R], cS: StringGenCache[S], cT: StringGenCache[T], cU: StringGenCache[U]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)] = {
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
        StringGenCachePath[Z](
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

  def apply22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Z](f: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => Z)(g: Z => (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V))(implicit t: StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)]): StringGenCache[Z] =
    t.xmap(x => f(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21, x._22))(g)

  implicit def tuple22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](cA: StringGenCache[A], cB: StringGenCache[B], cC: StringGenCache[C], cD: StringGenCache[D], cE: StringGenCache[E], cF: StringGenCache[F], cG: StringGenCache[G], cH: StringGenCache[H], cI: StringGenCache[I], cJ: StringGenCache[J], cK: StringGenCache[K], cL: StringGenCache[L], cM: StringGenCache[M], cN: StringGenCache[N], cO: StringGenCache[O], cP: StringGenCache[P], cQ: StringGenCache[Q], cR: StringGenCache[R], cS: StringGenCache[S], cT: StringGenCache[T], cU: StringGenCache[U], cV: StringGenCache[V]): StringGenCache[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] = {
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
        StringGenCachePath[Z](
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