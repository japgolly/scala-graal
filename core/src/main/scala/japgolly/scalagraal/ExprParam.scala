package japgolly.scalagraal

import java.util.function.Consumer
import org.graalvm.polyglot.proxy.Proxy
import org.graalvm.polyglot.{Context, Value}
import scala.runtime.AbstractFunction1

sealed trait ExprParam[A] {
  def contramap[B](f: B => A): ExprParam[B]

  final def narrow[B <: A]: ExprParam[B] =
    this.asInstanceOf[ExprParam[B]] // contramap(b => b)
}

object ExprParam {

  type RawValue = Any { type Raw = Unit }
  object RawValue extends AbstractFunction1[Any, RawValue] {
    override def apply(a: Any): RawValue = a.asInstanceOf[RawValue]
    val Null: RawValue = apply(null)
  }

  final case class SourceConst[A](source: String) extends ExprParam[A] {
    override def contramap[B](f: B => A) = this.asInstanceOf[ExprParam[B]]
  }

  final case class SourceFn[A](mkSource: A => String) extends ExprParam[A] {
    override def contramap[B](f: B => A) = SourceFn(mkSource compose f)
  }

  final case class ValueFn[A](mkValue: A => RawValue) extends ExprParam[A] {
    override def contramap[B](f: B => A) = ValueFn(mkValue compose f)
  }

  private[this] val rawValueFn = ValueFn[Any](RawValue)
  def RawValueFn[A] = rawValueFn.asInstanceOf[ValueFn[A]]

  final case class CtxValueFn[A](mkValue: A => Context => RawValue) extends ExprParam[A] {
    override def contramap[B](f: B => A) = CtxValueFn(mkValue compose f)
  }

  trait Contravariance {
    // Do this ourselves because Scala 2 chooses the most general implicit instance when the type param is
    // contravariant, instead of the most specific implicit instance. Thankfully Scala 3 will correct this.
    implicit final def exprParamContravariance[A <: AnyRef](implicit p: ExprParam[_ >: A]): ExprParam[A] = p.narrow
  }

  trait Primitives {
    implicit def exprParamBoolean: ValueFn[Boolean] = RawValueFn
    implicit def exprParamByte   : ValueFn[Byte   ] = RawValueFn
    implicit def exprParamShort  : ValueFn[Short  ] = RawValueFn
    implicit def exprParamInt    : ValueFn[Int    ] = RawValueFn
    implicit def exprParamLong   : ValueFn[Long   ] = RawValueFn
    implicit def exprParamFloat  : ValueFn[Float  ] = RawValueFn
    implicit def exprParamDouble : ValueFn[Double ] = RawValueFn
    implicit def exprParamString : ValueFn[String ] = RawValueFn
  }

  trait ArrayPrimitives {
    implicit def exprParamArrayBoolean: ValueFn[Array[Boolean]] = RawValueFn
    implicit def exprParamArrayByte   : ValueFn[Array[Byte   ]] = RawValueFn
    implicit def exprParamArrayShort  : ValueFn[Array[Short  ]] = RawValueFn
    implicit def exprParamArrayInt    : ValueFn[Array[Int    ]] = RawValueFn
    implicit def exprParamArrayLong   : ValueFn[Array[Long   ]] = RawValueFn
    implicit def exprParamArrayFloat  : ValueFn[Array[Float  ]] = RawValueFn
    implicit def exprParamArrayDouble : ValueFn[Array[Double ]] = RawValueFn
    implicit def exprParamArrayString : ValueFn[Array[String ]] = RawValueFn
  }

  trait JavaValues {
    implicit def exprConsumer[A]: ValueFn[Consumer[A]] = RawValueFn
  }

  trait PolyglotValues {
    implicit def exprParamValue: ValueFn[Value] = RawValueFn
    implicit def exprParamProxy: ValueFn[Proxy] = RawValueFn
  }

  trait Defaults
    extends Primitives
       with ArrayPrimitives
       with Contravariance
       with JavaValues
       with PolyglotValues
}

