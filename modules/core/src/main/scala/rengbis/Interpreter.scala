package rengbis

import zio.Chunk
import rengbis.Schema.{ Annotated, AlternativeValues, Deprecated, Documented, ListOfValues, MapValue, ObjectValue, Schema, TupleValue }
import rengbis.Schema.BinaryConstraint.BinaryToTextEncoder

import scala.util.{ Failure, Success, Try }
import java.time.{ LocalDate, LocalTime }
import java.time.format.DateTimeFormatter

object Interpreter:

    /** Walks the schema and value trees together, applying annotation-based coercion.
      *
      * Format annotations (`--#`) drive two kinds of coercion:
      *   - `encoding:` on binary types → decodes text (base64, hex, etc.) into BinaryValue
      *   - `format:` on time types → validates text against a time format
      *
      * This step runs BEFORE validation, producing coerced values that the Validator can then check constraints on.
      */
    def coerce(schema: Schema, value: Value): Either[String, Value] = schema match
        case Documented(_, inner)        => coerce(inner, value)
        case Deprecated(inner)           => coerce(inner, value)
        case Annotated(ann, inner)       => coerceAnnotated(ann, inner, value).flatMap(v => coerce(inner, v))
        case ObjectValue(obj)            => coerceObject(obj, value)
        case ListOfValues(s, _*)         => coerceList(s, value)
        case TupleValue(options*)        => coerceTuple(options, value)
        case AlternativeValues(options*) => coerceAlternatives(options, value)
        case MapValue(valueSchema)       => coerceMap(valueSchema, value)
        case _                           => Right(value)

    // ........................................................................

    private def coerceAnnotated(ann: FormatAnnotation.Annotations, schema: Schema, value: Value): Either[String, Value] =
        value match
            case Value.TextValue(text) =>
                ann.get("encoding") match
                    case Some(enc) => decodeBinaryEncoding(enc, text).map(Value.BinaryValue(_))
                    case None      =>
                        ann.get("format") match
                            case Some(fmt) => validateTimeFormat(fmt, text).map(_ => value)
                            case None      => Right(value)
            case _ => Right(value)

    private def coerceObject(obj: Map[Schema.ObjectLabel, Schema], value: Value): Either[String, Value] =
        value match
            case Value.ObjectWithValues(values) =>
                obj.foldLeft(Right(values): Either[String, Map[String, Value]]):
                    case (acc, (label, fieldSchema)) =>
                        acc.flatMap: vs =>
                            vs.get(label.label) match
                                case Some(v) => coerce(fieldSchema, v).map(cv => vs.updated(label.label, cv))
                                case None    => Right(vs)
                .map(Value.ObjectWithValues(_))
            case _ => Right(value)

    private def coerceList(itemSchema: Schema, value: Value): Either[String, Value] =
        value match
            case Value.ListOfValues(values) =>
                values
                    .foldLeft(Right(Chunk.empty[Value]): Either[String, Chunk[Value]]):
                        case (acc, v) => acc.flatMap(vs => coerce(itemSchema, v).map(vs :+ _))
                    .map(Value.ListOfValues(_))
            case _ => Right(value)

    private def coerceTuple(options: Seq[Schema], value: Value): Either[String, Value] =
        value match
            case Value.TupleOfValues(values) =>
                options
                    .zip(values)
                    .foldLeft(Right(Chunk.empty[Value]): Either[String, Chunk[Value]]):
                        case (acc, (s, v)) => acc.flatMap(vs => coerce(s, v).map(vs :+ _))
                    .map(Value.TupleOfValues(_))
            case Value.ListOfValues(values) =>
                options
                    .zip(values)
                    .foldLeft(Right(Chunk.empty[Value]): Either[String, Chunk[Value]]):
                        case (acc, (s, v)) => acc.flatMap(vs => coerce(s, v).map(vs :+ _))
                    .map(Value.TupleOfValues(_))
            case _ => Right(value)

    private def coerceAlternatives(options: Seq[Schema], value: Value): Either[String, Value] =
        options
            .map(s => coerce(s, value))
            .find(_.isRight)
            .getOrElse(Right(value))

    private def coerceMap(valueSchema: Schema, value: Value): Either[String, Value] =
        value match
            case Value.ObjectWithValues(values) =>
                values
                    .foldLeft(Right(Map.empty[String, Value]): Either[String, Map[String, Value]]):
                        case (acc, (k, v)) => acc.flatMap(vs => coerce(valueSchema, v).map(cv => vs + (k -> cv)))
                    .map(Value.ObjectWithValues(_))
            case _ => Right(value)

    // ........................................................................
    // Time format validation

    private val namedFormats: Map[String, DateTimeFormatter] = Map(
        "iso8601"          -> DateTimeFormatter.ISO_LOCAL_DATE_TIME,
        "iso8601-datetime" -> DateTimeFormatter.ISO_LOCAL_DATE_TIME,
        "iso8601-date"     -> DateTimeFormatter.ISO_LOCAL_DATE,
        "iso8601-time"     -> DateTimeFormatter.ISO_LOCAL_TIME,
        "rfc3339"          -> DateTimeFormatter.ISO_OFFSET_DATE_TIME
    )

    private def validateTimeFormat(format: String, text: String): Either[String, Unit] =
        val formatter = namedFormats.get(format) match
            case Some(f) => Right(f)
            case None    =>
                Try(DateTimeFormatter.ofPattern(format)) match
                    case Success(f) => Right(f)
                    case Failure(e) => Left(s"Invalid time format '$format': ${ e.getMessage }")

        formatter.flatMap: fmt =>
            Try(LocalTime.parse(text, fmt))
                .orElse(Try(LocalDate.parse(text, fmt)))
                .orElse(Try(java.time.LocalDateTime.parse(text, fmt))) match
                case Success(_) => Right(())
                case Failure(e) => Left(s"time format $format not matching: ${ e.getMessage }")

    // ........................................................................
    // Binary encoding

    private def decodeBinaryEncoding(encoding: String, text: String): Either[String, Chunk[Byte]] =
        val encoder = BinaryToTextEncoder.values.find(_.code == encoding)
        encoder match
            case Some(enc) => BinaryDecoding.decode(enc, text)
            case None      => Left(s"Unknown encoding: $encoding")

    object BinaryDecoding:
        def decode(encoder: BinaryToTextEncoder, text: String): Either[String, Chunk[Byte]] =
            Try:
                encoder match
                    case BinaryToTextEncoder.base64  =>
                        Chunk.fromArray(java.util.Base64.getDecoder.decode(text))
                    case BinaryToTextEncoder.hex     =>
                        val cleanHex = text.replaceAll("\\s", "")
                        if cleanHex.length % 2 != 0 then throw new IllegalArgumentException("Hex string must have even length")
                        Chunk.fromArray(cleanHex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
                    case BinaryToTextEncoder.base32  =>
                        val alphabet   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
                        val cleanInput = text.toUpperCase.replaceAll("\\s", "").replaceAll("=", "")
                        val bits       = cleanInput.flatMap { c =>
                            val idx = alphabet.indexOf(c)
                            if idx < 0 then throw new IllegalArgumentException(s"Invalid base32 character: $c")
                            (4 to 0 by -1).map(i => (idx >> i) & 1)
                        }
                        Chunk.fromArray(bits.grouped(8).filter(_.length == 8).map(_.foldLeft(0)((acc, b) => (acc << 1) | b).toByte).toArray)
                    case BinaryToTextEncoder.base58  =>
                        val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
                        var num      = BigInt(0)
                        for c <- text do
                            val idx = alphabet.indexOf(c)
                            if idx < 0 then throw new IllegalArgumentException(s"Invalid base58 character: $c")
                            num = num * 58 + idx
                        val bytes  = num.toByteArray
                        val result = if bytes.length > 1 && bytes(0) == 0 then bytes.tail else bytes
                        Chunk.fromArray(result)
                    case BinaryToTextEncoder.ascii85 =>
                        val cleanInput = text.replaceAll("\\s", "")
                        val input      =
                            if cleanInput.startsWith("<~") && cleanInput.endsWith("~>")
                            then cleanInput.drop(2).dropRight(2)
                            else cleanInput
                        val result = scala.collection.mutable.ArrayBuffer[Byte]()
                        var i      = 0
                        while i < input.length do
                            if input(i) == 'z' then
                                result ++= Array[Byte](0, 0, 0, 0)
                                i += 1
                            else
                                val chunk       = input.slice(i, i + 5).padTo(5, 'u')
                                var value       = 0L
                                for c <- chunk do
                                    if c < '!' || c > 'u' then throw new IllegalArgumentException(s"Invalid ascii85 character: $c")
                                    value = value * 85 + (c - '!')
                                val bytesToTake = Math.min(4, input.length - i - 1).max(1)
                                val bytes       = (0 until 4).map(j => ((value >> (24 - 8 * j)) & 0xff).toByte).toArray
                                result ++= bytes.take(bytesToTake)
                                i += Math.min(5, input.length - i)
                        Chunk.fromArray(result.toArray)
            match
                case Success(bytes) => Right(bytes)
                case Failure(e)     => Left(e.getMessage)
