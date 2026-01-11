package rengbis

import zio.Chunk

sealed abstract class Value:
    def valueTypeDescription: String

object Value:
    final case class Fail() extends Value:
        def valueTypeDescription: String = s"Fail"

    final case class BooleanValue(value: Boolean) extends Value:
        def valueTypeDescription: String = s"Boolean"

    final case class NumberValue(value: BigDecimal) extends Value:
        def valueTypeDescription: String = s"Number"

    final case class TextValue(value: String) extends Value:
        def valueTypeDescription: String = s"Text"

    final case class BinaryValue(value: Chunk[Byte]) extends Value:
        def valueTypeDescription: String = s"Binary"

    final case class ListOfValues(values: Chunk[Value]) extends Value:
        def valueTypeDescription: String = s"List"

    final case class TupleOfValues(values: Chunk[Value]) extends Value:
        def valueTypeDescription: String = s"Tuple"

    final case class ObjectWithValues(values: Map[String, Value]) extends Value:
        def valueTypeDescription: String = s"Object"

    final case class NullValue() extends Value:
        def valueTypeDescription: String = s"Null"
