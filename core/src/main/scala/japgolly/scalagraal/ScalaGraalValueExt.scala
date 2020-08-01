package japgolly.scalagraal

import org.graalvm.polyglot.{Language => _, _}

class ScalaGraalValueExt(private val self: Value) extends AnyVal {

  def arrayIterator(): Iterator[Value] =
    (0L until self.getArraySize).iterator.map(self.getArrayElement(_))

  def toBooleanArray(): Array[Boolean] = arrayIterator().map(_.asBoolean()).toArray
  def toByteArray   (): Array[Byte   ] = arrayIterator().map(_.asByte   ()).toArray
  def toDoubleArray (): Array[Double ] = arrayIterator().map(_.asDouble ()).toArray
  def toFloatArray  (): Array[Float  ] = arrayIterator().map(_.asFloat  ()).toArray
  def toIntArray    (): Array[Int    ] = arrayIterator().map(_.asInt    ()).toArray
  def toLongArray   (): Array[Long   ] = arrayIterator().map(_.asLong   ()).toArray
  def toShortArray  (): Array[Short  ] = arrayIterator().map(_.asShort  ()).toArray
  def toStringArray (): Array[String ] = arrayIterator().map(_.asString ()).toArray

  def toBooleanList(): List[Boolean] = arrayIterator().map(_.asBoolean()).toList
  def toByteList   (): List[Byte   ] = arrayIterator().map(_.asByte   ()).toList
  def toDoubleList (): List[Double ] = arrayIterator().map(_.asDouble ()).toList
  def toFloatList  (): List[Float  ] = arrayIterator().map(_.asFloat  ()).toList
  def toIntList    (): List[Int    ] = arrayIterator().map(_.asInt    ()).toList
  def toLongList   (): List[Long   ] = arrayIterator().map(_.asLong   ()).toList
  def toShortList  (): List[Short  ] = arrayIterator().map(_.asShort  ()).toList
  def toStringList (): List[String ] = arrayIterator().map(_.asString ()).toList

  def toBooleanVector(): Vector[Boolean] = arrayIterator().map(_.asBoolean()).toVector
  def toByteVector   (): Vector[Byte   ] = arrayIterator().map(_.asByte   ()).toVector
  def toDoubleVector (): Vector[Double ] = arrayIterator().map(_.asDouble ()).toVector
  def toFloatVector  (): Vector[Float  ] = arrayIterator().map(_.asFloat  ()).toVector
  def toIntVector    (): Vector[Int    ] = arrayIterator().map(_.asInt    ()).toVector
  def toLongVector   (): Vector[Long   ] = arrayIterator().map(_.asLong   ()).toVector
  def toShortVector  (): Vector[Short  ] = arrayIterator().map(_.asShort  ()).toVector
  def toStringVector (): Vector[String ] = arrayIterator().map(_.asString ()).toVector
}
