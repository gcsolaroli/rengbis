package rengbis

import java.nio.file.Path
import zio.Chunk
import rengbis.Schema.{ AlternativeValues, AnyValue, BooleanValue, EnumValues, Fail, GivenTextValue, ImportStatement, ListOfValues, MandatoryLabel, MapValue, NamedValueReference, NumericValue, ObjectValue, OptionalLabel, Schema, ScopedReference, TextValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, BoundOp, ListConstraint, NumericConstraint, TextConstraint }
import rengbis.Schema.BinaryConstraint.BinaryToTextEncoder
import rengbis.Schema.BinaryValue as SchemaBinaryValue
import scala.util.{ Failure, Success, Try }

object Validator:
    case class Valid()
    case class ValidationError(message: String)

    final case class ValidationResult(value: Valid | Chunk[ValidationError]):
        def isValid: Boolean               = value match
            case v: Valid                  => true
            case v: Chunk[ValidationError] => false
        def errorMessage: String           = value match
            case _: Valid                       => ""
            case errors: Chunk[ValidationError] => errors.map(_.message).mkString("\n")
        def toEither: Either[String, Unit] = value match
            case v: Valid                  => Right(())
            case v: Chunk[ValidationError] => Left(v.map(_.message).mkString("\n"))

    object ValidationResult:
        def reportError(message: String): ValidationResult                  = ValidationResult(Chunk(ValidationError(message)))
        val valid: ValidationResult                                         = ValidationResult(Valid())
        def summarize(values: Iterable[ValidationResult]): ValidationResult =
            val errors: Iterable[ValidationError] = values.flatMap(v =>
                v.value match
                    case errors: Chunk[ValidationError] => errors
                    case _                              => Chunk()
            )
            if errors.isEmpty then ValidationResult.valid
            else ValidationResult(Chunk.fromIterable(errors))

    def validate(validator: ((Schema) => Either[String, Value]))(schema: Schema): ValidationResult = validator(schema) match
        case Left(message) => ValidationResult.reportError(message)
        case Right(value)  => validateValue(schema, value)

    // ========================================================================

    def formatToRegex(format: String): scala.util.matching.Regex =
        val sb = new StringBuilder
        var i  = 0
        while i < format.length do
            format(i) match
                case '#' => sb.append("""\p{N}""")             //  [0-9]
                case 'X' => sb.append("""\p{Letter}""")        //  [a-zA-Z]
                case '@' => sb.append("""[\p{Letter}\p{N}]""") //  [a-zA-Z0-9]
                case '*' => sb.append(""".""")
                case c   => sb.append("""\""").append(c)
            i += 1
        return sb.toString.r

    def validateTextConstraints(constraint: TextConstraint.Constraint, text: String) = constraint match
        case TextConstraint.Size(bound)    =>
            val len = text.length
            bound.op match
                case BoundOp.Exact        => if len == bound.value then ValidationResult.valid else ValidationResult.reportError(s"length constraint (${ bound.value }) not met: ${ len }")
                case BoundOp.MinInclusive => if len >= bound.value then ValidationResult.valid else ValidationResult.reportError(s"minLength constraint (${ bound.value }) not met: ${ len }")
                case BoundOp.MinExclusive => if len > bound.value then ValidationResult.valid else ValidationResult.reportError(s"minLength constraint (> ${ bound.value }) not met: ${ len }")
                case BoundOp.MaxInclusive => if len <= bound.value then ValidationResult.valid else ValidationResult.reportError(s"maxLength constraint (${ bound.value }) not met: ${ len }")
                case BoundOp.MaxExclusive => if len < bound.value then ValidationResult.valid else ValidationResult.reportError(s"maxLength constraint (< ${ bound.value }) not met: ${ len }")
        case TextConstraint.Regex(pattern) => if pattern.r.matches(text) then ValidationResult.valid else ValidationResult.reportError(s"regex ($pattern) not matching")
        case TextConstraint.Format(format) => if formatToRegex(format).matches(text) then ValidationResult.valid else ValidationResult.reportError(s"format (${ format }) not matching")

    def validateListConstraints(constraint: ListConstraint.Constraint, list: Chunk[Value], itemSchema: Schema): ValidationResult =
        val size = list.size
        constraint match
            case ListConstraint.Size(bound)            =>
                bound.op match
                    case BoundOp.Exact        => if size == bound.value then ValidationResult.valid else ValidationResult.reportError(s"list exact size constraint (${ bound.value }) not met: ${ size }")
                    case BoundOp.MinInclusive => if size >= bound.value then ValidationResult.valid else ValidationResult.reportError(s"list minimum size constraint (${ bound.value }) not met: ${ size }")
                    case BoundOp.MinExclusive => if size > bound.value then ValidationResult.valid else ValidationResult.reportError(s"list minimum size constraint (> ${ bound.value }) not met: ${ size }")
                    case BoundOp.MaxInclusive => if size <= bound.value then ValidationResult.valid else ValidationResult.reportError(s"list maximum size constraint (${ bound.value }) not met: ${ size }")
                    case BoundOp.MaxExclusive => if size < bound.value then ValidationResult.valid else ValidationResult.reportError(s"list maximum size constraint (< ${ bound.value }) not met: ${ size }")
            case ListConstraint.Unique                 => validateUniqueness(list, itemSchema)
            case ListConstraint.UniqueByFields(fields) => validateUniquenessBy(list, fields, itemSchema)

    private def normalizeValueForUniquenessComparison(value: Value, schema: Schema): Option[Any] = schema match
        case NumericValue(_*) =>
            value match
                case Value.NumberValue(v) => Some(v)
                case Value.TextValue(v)   => Try(BigDecimal(v)).toOption
                case _                    => None
        case TextValue(_*)    =>
            value match
                case Value.TextValue(v) => Some(v)
                case _                  => None
        case BooleanValue()   =>
            value match
                case Value.BooleanValue(v) => Some(v)
                case _                     => None
        case _                => extractSimpleKey(value)

    private def extractSimpleKey(value: Value): Option[Any] = value match
        case Value.TextValue(v)    => Some(v)
        case Value.NumberValue(v)  => Some(v)
        case Value.BooleanValue(v) => Some(v)
        case _                     => None

    private def extractFieldKey(value: Value, fields: Seq[String], itemSchema: Schema): Option[Seq[Any]] =
        val fieldSchemas: Map[String, Schema] = itemSchema match
            case ObjectValue(obj) => obj.map((label, schema) => (label.label, schema))
            case _                => Map.empty

        value match
            case Value.ObjectWithValues(values) =>
                val keys = fields.flatMap { f =>
                    for
                        fieldValue  <- values.get(f)
                        fieldSchema <- fieldSchemas
                                           .get(f)
                                           .orElse(Some(fieldValue match
                                               case Value.TextValue(_)    => TextValue()
                                               case Value.NumberValue(_)  => NumericValue()
                                               case Value.BooleanValue(_) => BooleanValue()
                                               case _                     => Fail()))
                        key         <- normalizeValueForUniquenessComparison(fieldValue, fieldSchema)
                    yield key
                }
                if keys.size == fields.size then Some(keys) else None
            case _                              => None

    private def validateUniqueness(list: Chunk[Value], itemSchema: Schema): ValidationResult =
        val keys = list.flatMap(v => normalizeValueForUniquenessComparison(v, itemSchema))
        if keys.size != list.size then ValidationResult.reportError("uniqueness constraint only applies to simple values (text, number, boolean)")
        else
            val duplicates = keys.groupBy(identity).filter(_._2.size > 1).keys.toSeq
            if duplicates.nonEmpty then ValidationResult.reportError(s"duplicate values found: ${ duplicates.mkString(", ") }")
            else ValidationResult.valid

    private def validateUniquenessBy(list: Chunk[Value], fields: Seq[String], itemSchema: Schema): ValidationResult =
        val keys = list.flatMap(v => extractFieldKey(v, fields, itemSchema))
        if keys.size != list.size then ValidationResult.reportError(s"uniqueness constraint: all items must be objects with fields: ${ fields.mkString(", ") }")
        else
            val duplicates = keys.groupBy(identity).filter(_._2.size > 1).keys.toSeq
            if duplicates.nonEmpty then ValidationResult.reportError(s"duplicate values found for fields (${ fields.mkString(", ") }): ${ duplicates.map(_.mkString("(", ", ", ")")).mkString(", ") }")
            else ValidationResult.valid

    def validateNumericConstraints(constraint: NumericConstraint.Constraint, value: BigDecimal) = constraint match
        case NumericConstraint.Integer      => if value.isWhole then ValidationResult.valid else ValidationResult.reportError(s"integer constraint not met: ${ value } is not an integer")
        case NumericConstraint.Value(bound) =>
            bound.op match
                case BoundOp.Exact        => if value == bound.value then ValidationResult.valid else ValidationResult.reportError(s"exact value constraint (${ bound.value }) not met: ${ value }")
                case BoundOp.MinInclusive => if value >= bound.value then ValidationResult.valid else ValidationResult.reportError(s"minimum value constraint (>= ${ bound.value }) not met: ${ value }")
                case BoundOp.MinExclusive => if value > bound.value then ValidationResult.valid else ValidationResult.reportError(s"minimum value constraint (> ${ bound.value }) not met: ${ value }")
                case BoundOp.MaxInclusive => if value <= bound.value then ValidationResult.valid else ValidationResult.reportError(s"maximum value constraint (<= ${ bound.value }) not met: ${ value }")
                case BoundOp.MaxExclusive => if value < bound.value then ValidationResult.valid else ValidationResult.reportError(s"maximum value constraint (< ${ bound.value }) not met: ${ value }")

    def decodeEncoding(encoder: BinaryToTextEncoder, text: String): Either[String, Chunk[Byte]] =
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
                    val bytes    = num.toByteArray
                    val result   = if bytes.length > 1 && bytes(0) == 0 then bytes.tail else bytes
                    Chunk.fromArray(result)
                case BinaryToTextEncoder.ascii85 =>
                    val cleanInput = text.replaceAll("\\s", "")
                    val input      =
                        if cleanInput.startsWith("<~") && cleanInput.endsWith("~>")
                        then cleanInput.drop(2).dropRight(2)
                        else cleanInput
                    val result     = scala.collection.mutable.ArrayBuffer[Byte]()
                    var i          = 0
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

    def validateBinarySizeConstraint(constraint: BinaryConstraint.Size, data: Chunk[Byte]): ValidationResult =
        val byteSize = data.length
        val bound    = constraint.bound
        val unit     = constraint.unit
        val size     = bound.value
        bound.op match
            case BoundOp.Exact        => if byteSize == (size * unit.bytes) then ValidationResult.valid else ValidationResult.reportError(s"binary exact size constraint (${ size } ${ unit.symbol }) not met: ${ byteSize / unit.bytes }")
            case BoundOp.MinInclusive => if byteSize >= (size * unit.bytes) then ValidationResult.valid else ValidationResult.reportError(s"binary minimum size constraint (${ size } ${ unit.symbol }) not met: ${ byteSize / unit.bytes }")
            case BoundOp.MinExclusive => if byteSize > (size * unit.bytes) then ValidationResult.valid else ValidationResult.reportError(s"binary minimum size constraint (> ${ size } ${ unit.symbol }) not met: ${ byteSize / unit.bytes }")
            case BoundOp.MaxInclusive => if byteSize <= (size * unit.bytes) then ValidationResult.valid else ValidationResult.reportError(s"binary maximum size constraint (${ size } ${ unit.symbol }) not met: ${ byteSize / unit.bytes }")
            case BoundOp.MaxExclusive => if byteSize < (size * unit.bytes) then ValidationResult.valid else ValidationResult.reportError(s"binary maximum size constraint (< ${ size } ${ unit.symbol }) not met: ${ byteSize / unit.bytes }")

    def validateBinaryConstraints(constraints: Seq[BinaryConstraint.Constraint], data: Chunk[Byte]): ValidationResult =
        val sizeConstraints = constraints.collect { case c: BinaryConstraint.Size => c }
        ValidationResult.summarize(sizeConstraints.map(c => validateBinarySizeConstraint(c, data)))

    def validateValue(schema: Schema, value: Value): ValidationResult = schema match
        case Fail()                          => ValidationResult.reportError(s"fail value")
        case AnyValue()                      => ValidationResult.valid
        case BooleanValue()                  =>
            value match
                case Value.BooleanValue(value) => ValidationResult.valid
                case _                         => ValidationResult.reportError(s"expected boolean value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case TextValue(constraints*)         =>
            value match
                case Value.TextValue(text) => ValidationResult.summarize(constraints.map(c => validateTextConstraints(c, text)))
                case _                     => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case GivenTextValue(givenValue)      =>
            value match
                case Value.TextValue(value) => if (value == givenValue) then ValidationResult.valid else ValidationResult.reportError(s"expected value '${ givenValue }', found '${ value }'")
                case _                      => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case NumericValue(constraints*)      =>
            value match
                case Value.NumberValue(numValue) => ValidationResult.summarize(constraints.map(c => validateNumericConstraints(c, numValue)))
                case Value.TextValue(textValue)  =>
                    Try(BigDecimal(textValue)) match
                        case Success(numValue) => ValidationResult.summarize(constraints.map(c => validateNumericConstraints(c, numValue)))
                        case Failure(e)        => ValidationResult.reportError(e.getMessage())
                case _                           => ValidationResult.reportError(s"expected numeric value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case SchemaBinaryValue(constraints*) =>
            value match
                case Value.BinaryValue(data) =>
                    // Direct binary data - validate size constraints
                    validateBinaryConstraints(constraints, data)
                case Value.TextValue(text)   =>
                    constraints.collectFirst { case BinaryConstraint.Encoding(enc) => enc } match
                        case Some(encoding) =>
                            decodeEncoding(encoding, text) match
                                case Right(bytes) => validateBinaryConstraints(constraints, bytes)
                                case Left(error)  => ValidationResult.reportError(s"Failed to decode $encoding: $error")
                        case None           =>
                            ValidationResult.valid
                case _                       => ValidationResult.reportError(s"expected binary value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case EnumValues(values*)             =>
            value match
                case Value.TextValue(value) => if (values.contains(value)) then ValidationResult.valid else ValidationResult.reportError(s"enum type does not include provided value: '${ value }'")
                case _                      => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case ListOfValues(s, constraints*)   =>
            value match
                case Value.ListOfValues(values) =>
                    ValidationResult.summarize(
                        constraints.map(c => validateListConstraints(c, values, s)) ++
                            values.map(v => validateValue(s, v))
                    )
                case value                      => ValidationResult.reportError(s"expected list of values; ${ value.valueTypeDescription } found [value: ${ value }]")
        case TupleValue(options*)            =>
            value match
                case Value.TupleOfValues(values) => ValidationResult.summarize(options.zipAll(values, Schema.Fail(), Value.Fail()).map((s, v) => validateValue(s, v)))
                case Value.ListOfValues(values)  => ValidationResult.summarize(options.zipAll(values, Schema.Fail(), Value.Fail()).map((s, v) => validateValue(s, v)))
                case value                       => ValidationResult.reportError(s"expected tuple of values; ${ value.valueTypeDescription } found [value: ${ value }]")
        case AlternativeValues(options*)     =>
            options
                .map(s => validateValue(s, value))
                .find(result => result.isValid)
                .getOrElse(ValidationResult.reportError(s"could not match value ${ value } with any of the available options"))
        case ObjectValue(obj)                =>
            value match
                case Value.ObjectWithValues(values) =>
                    ValidationResult.summarize(
                        obj.map((l, s) =>
                            values
                                .get(l.label)
                                .map(v => validateValue(s, v))
                                .getOrElse(l match
                                    case MandatoryLabel(label) => ValidationResult.reportError(s"Value is missing expected key ${ label }")
                                    case OptionalLabel(label)  => ValidationResult.valid)
                        )
                    )
                case value                          => ValidationResult.reportError(s"expected object value; ${ value.valueTypeDescription } found")
        case MapValue(valueSchema)           =>
            value match
                case Value.ObjectWithValues(values) => ValidationResult.summarize(values.values.map(v => validateValue(valueSchema, v)))
                case value                          => ValidationResult.reportError(s"expected map/object value; ${ value.valueTypeDescription } found")
        case NamedValueReference(_)          => ValidationResult.reportError(s"#WTF: unresolved NamedValueReference still present")
        case ImportStatement(_, _)           => ValidationResult.reportError(s"#WTF: unresolved ImportStatement still present")
        case ScopedReference(_, _)           => ValidationResult.reportError(s"#WTF: unresolved ScopedReference still present")
