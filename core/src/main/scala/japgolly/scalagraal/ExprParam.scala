package japgolly.scalagraal

import org.graalvm.polyglot.Context
import org.graalvm.polyglot.proxy.Proxy

sealed trait ExprParam[A] {
  def contramap[B](f: B => A): ExprParam[B]

  final def narrow[B <: A]: ExprParam[B] =
    this.asInstanceOf[ExprParam[B]] // contramap(b => b)
}

object ExprParam {

  final case class SourceConst[A](source: String) extends ExprParam[A] {
    override def contramap[B](f: B => A) = this.asInstanceOf[ExprParam[B]]
  }

  final case class SourceFn[A](mkSource: A => String) extends ExprParam[A] {
    override def contramap[B](f: B => A) = SourceFn(mkSource compose f)
  }

  final case class ValueFn[A](mkValue: A => Any) extends ExprParam[A] {
    override def contramap[B](f: B => A) = ValueFn(mkValue compose f)
  }

  final case class CtxValueFn[A](mkValue: A => Context => Any) extends ExprParam[A] {
    override def contramap[B](f: B => A) = CtxValueFn(mkValue compose f)
  }

  private[this] val _id: Any => Any = identity
  private def id[A] = _id.asInstanceOf[A => A]

  trait Contravariance {
    // Do this ourselves because Scala 2 chooses the most general implicit instance when the type param is
    // contravariant, instead of the most specific implicit instance. Thankfully Scala 3 will correct this.
    implicit final def exprParamContravariance[B <: A, A](implicit p: ExprParam[A]): ExprParam[B] = p.narrow
  }

  trait Primitives {
    implicit val exprParamBoolean: ExprParam[Boolean] = ValueFn(id)
    implicit val exprParamByte   : ExprParam[Byte   ] = ValueFn(id)
    implicit val exprParamShort  : ExprParam[Short  ] = ValueFn(id)
    implicit val exprParamInt    : ExprParam[Int    ] = ValueFn(id)
    implicit val exprParamLong   : ExprParam[Long   ] = ValueFn(id)
    implicit val exprParamFloat  : ExprParam[Float  ] = ValueFn(id)
    implicit val exprParamDouble : ExprParam[Double ] = ValueFn(id)
    implicit val exprParamString : ExprParam[String ] = ValueFn(id)
  }

  trait ArrayPrimitives {
    implicit val exprParamArrayBoolean: ExprParam[Array[Boolean]] = ValueFn(id)
    implicit val exprParamArrayByte   : ExprParam[Array[Byte   ]] = ValueFn(id)
    implicit val exprParamArrayShort  : ExprParam[Array[Short  ]] = ValueFn(id)
    implicit val exprParamArrayInt    : ExprParam[Array[Int    ]] = ValueFn(id)
    implicit val exprParamArrayLong   : ExprParam[Array[Long   ]] = ValueFn(id)
    implicit val exprParamArrayFloat  : ExprParam[Array[Float  ]] = ValueFn(id)
    implicit val exprParamArrayDouble : ExprParam[Array[Double ]] = ValueFn(id)
    implicit val exprParamArrayString : ExprParam[Array[String ]] = ValueFn(id)
  }

  trait PolyglotValues {
    implicit val exprParamProxy: ExprParam[Proxy] = ValueFn(id)
  }

  trait Defaults
    extends Primitives
       with ArrayPrimitives
       with Contravariance
}

