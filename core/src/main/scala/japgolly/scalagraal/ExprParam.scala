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
    implicit final def exprParamContravariance[A <: AnyRef](implicit p: ExprParam[_ >: A]): ExprParam[A] = p.narrow
  }

  trait Primitives {
    implicit val exprParamBoolean: ValueFn[Boolean] = ValueFn(id)
    implicit val exprParamByte   : ValueFn[Byte   ] = ValueFn(id)
    implicit val exprParamShort  : ValueFn[Short  ] = ValueFn(id)
    implicit val exprParamInt    : ValueFn[Int    ] = ValueFn(id)
    implicit val exprParamLong   : ValueFn[Long   ] = ValueFn(id)
    implicit val exprParamFloat  : ValueFn[Float  ] = ValueFn(id)
    implicit val exprParamDouble : ValueFn[Double ] = ValueFn(id)
    implicit val exprParamString : ValueFn[String ] = ValueFn(id)
  }

  trait ArrayPrimitives {
    implicit val exprParamArrayBoolean: ValueFn[Array[Boolean]] = ValueFn(id)
    implicit val exprParamArrayByte   : ValueFn[Array[Byte   ]] = ValueFn(id)
    implicit val exprParamArrayShort  : ValueFn[Array[Short  ]] = ValueFn(id)
    implicit val exprParamArrayInt    : ValueFn[Array[Int    ]] = ValueFn(id)
    implicit val exprParamArrayLong   : ValueFn[Array[Long   ]] = ValueFn(id)
    implicit val exprParamArrayFloat  : ValueFn[Array[Float  ]] = ValueFn(id)
    implicit val exprParamArrayDouble : ValueFn[Array[Double ]] = ValueFn(id)
    implicit val exprParamArrayString : ValueFn[Array[String ]] = ValueFn(id)
  }

  trait PolyglotValues {
    implicit val exprParamProxy: ValueFn[Proxy] = ValueFn(id)
  }

  trait Defaults
    extends Primitives
       with ArrayPrimitives
       with Contravariance
}

