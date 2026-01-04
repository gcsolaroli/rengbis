package rengbis

import zio.Chunk
import rengbis.Schema.*
import scala.util.{ Failure, Success, Try }

object Validator:
    case class Valid()
    case class ValidationError(message: String)

    final case class ValidationResult(value: Valid | Chunk[ValidationError]):
        def isValid: Boolean     = value match
            case v: Valid                  => true
            case v: Chunk[ValidationError] => false
        def errorMessage: String = value match
            case _: Valid                       => ""
            case errors: Chunk[ValidationError] => errors.map(_.message).mkString("\n")

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

    def validateString(dataParser: (String) => Either[String, Value])(schema: Schema, string: String): ValidationResult = dataParser(string) match
        case Left(message)    => ValidationResult.reportError(message)
        case Right(jsonValue) => validateValue(schema, jsonValue)

    // ========================================================================

    def formatToRegex(format: String): scala.util.matching.Regex =
        val sb     = new StringBuilder
        var i      = 0
        while i < format.length do
            format(i) match
                // case '#' => sb.append("""[0-9]""")
                case '#' => sb.append("""\p{N}""")
                // case 'X' => sb.append("""[a-zA-Z]""")
                case 'X' => sb.append("""\p{Letter}""")
                // case '@' => sb.append("""[a-zA-Z0-9]""")
                case '@' => sb.append("""[\p{Letter}\p{N}]""")
                case '*' => sb.append(""".""")
                // case '-' | '/' | ' ' => sb.append("\\").append(format(i))
                case c   => sb.append("""\""").append(c)
            i += 1
        val result = sb.toString.r
        println(s"formatToRegex: ${ format } => '${ result }'")
        result

    def validateTextConstraints(constraint: TextConstraint.Constraint, text: String) = constraint match
        case TextConstraint.MinLength(size) => if (text.length >= size) then ValidationResult.valid else ValidationResult.reportError(s"minLength constraint (${ size }) not met: ${ text.length() }")
        case TextConstraint.MaxLength(size) => if (text.length <= size) then ValidationResult.valid else ValidationResult.reportError(s"maxLength constraint (${ size }) not met: ${ text.length() }")
        case TextConstraint.Length(size)    => if (text.length == size) then ValidationResult.valid else ValidationResult.reportError(s"length constraint (${ size }) not met: ${ text.length() }")
        case TextConstraint.Regex(pattern)  => if pattern.r.matches(text) then ValidationResult.valid else ValidationResult.reportError(s"regex ($pattern) not matching")
        case TextConstraint.Format(format)  => if formatToRegex(format).matches(text) then ValidationResult.valid else ValidationResult.reportError(s"format (${ format }) not matching")

    def validateListConstraints[A](constraint: ListConstraint.Constraint, list: Chunk[A]) =
        val size = list.size
        constraint match
            case ListConstraint.MinSize(minSize)     => if (size >= minSize) then ValidationResult.valid else ValidationResult.reportError(s"list minimum size constraint (${ minSize }) not met: ${ size }")
            case ListConstraint.MaxSize(maxSize)     => if (size <= maxSize) then ValidationResult.valid else ValidationResult.reportError(s"list maximum size constraint (${ maxSize }) not met: ${ size }")
            case ListConstraint.ExactSize(exactSize) => if (size == exactSize) then ValidationResult.valid else ValidationResult.reportError(s"list exact size constraint (${ exactSize }) not met: ${ size }")

    def validateNumericConstraints(constraint: NumericConstraint.Constraint, value: BigDecimal) = constraint match
        case NumericConstraint.Integer                => if value.isWhole then ValidationResult.valid else ValidationResult.reportError(s"integer constraint not met: ${ value } is not an integer")
        case NumericConstraint.MinValue(min)          => if value >= min then ValidationResult.valid else ValidationResult.reportError(s"minimum value constraint (>= ${ min }) not met: ${ value }")
        case NumericConstraint.MinValueExclusive(min) => if value > min then ValidationResult.valid else ValidationResult.reportError(s"minimum value constraint (> ${ min }) not met: ${ value }")
        case NumericConstraint.MaxValue(max)          => if value <= max then ValidationResult.valid else ValidationResult.reportError(s"maximum value constraint (<= ${ max }) not met: ${ value }")
        case NumericConstraint.MaxValueExclusive(max) => if value < max then ValidationResult.valid else ValidationResult.reportError(s"maximum value constraint (< ${ max }) not met: ${ value }")
        case NumericConstraint.ExactValue(exact)      => if value == exact then ValidationResult.valid else ValidationResult.reportError(s"exact value constraint (${ exact }) not met: ${ value }")

    def validateValue(schema: Schema, value: Value): ValidationResult = schema match
        case Fail()                        => ValidationResult.reportError(s"fail value")
        case BooleanValue()                =>
            value match
                case Value.BooleanValue(value) => ValidationResult.valid
                case _                         => ValidationResult.reportError(s"expected boolean value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case TextValue(constraints*)       =>
            value match
                case Value.TextValue(text) => ValidationResult.summarize(constraints.map(c => validateTextConstraints(c, text)))
                case _                     => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case GivenTextValue(givenValue)    =>
            value match
                case Value.TextValue(value) => if (value == givenValue) then ValidationResult.valid else ValidationResult.reportError(s"expected value '${ givenValue }', found '${ value }'")
                case _                      => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case NumericValue(constraints*)    =>
            value match
                case Value.NumberValue(numValue) => ValidationResult.summarize(constraints.map(c => validateNumericConstraints(c, numValue)))
                case Value.TextValue(textValue)  =>
                    Try(BigDecimal(textValue)) match
                        case Success(numValue) => ValidationResult.summarize(constraints.map(c => validateNumericConstraints(c, numValue)))
                        case Failure(e)        => ValidationResult.reportError(e.getMessage())
                case _                           => ValidationResult.reportError(s"expected numeric value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case EnumValues(values*)           =>
            value match
                case Value.TextValue(value) => if (values.contains(value)) then ValidationResult.valid else ValidationResult.reportError(s"enum type does not include provided value: '${ value }'")
                case _                      => ValidationResult.reportError(s"expected text value; ${ value.valueTypeDescription } found [value: ${ value }]")
        case ListOfValues(s, constraints*) =>
            value match
                case Value.ArrayOfValues(values) =>
                    ValidationResult.summarize(
                        constraints.map(c => validateListConstraints(c, values)) ++
                            values.map(v => validateValue(s, v))
                    )
                case value                       => ValidationResult.reportError(s"expected list of values; ${ value.valueTypeDescription } found [value: ${ value }]")
        case TupleValue(options*)          =>
            value match
                case Value.TupleOfValues(values) => ValidationResult.summarize(options.zipAll(values, Schema.Fail(), Value.Fail()).map((s, v) => validateValue(s, v)))
                case Value.ArrayOfValues(values) => ValidationResult.summarize(options.zipAll(values, Schema.Fail(), Value.Fail()).map((s, v) => validateValue(s, v)))
                case value                       => ValidationResult.reportError(s"expected tuple of values; ${ value.valueTypeDescription } found [value: ${ value }]")
        case AlternativeValues(options*)   =>
            options
                .map(s => validateValue(s, value))
                .find(result => result.isValid)
                .getOrElse(ValidationResult.reportError(s"could not match value ${ value } with any of the available options"))
        case ObjectValue(obj)              =>
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
        case MapValue(valueSchema)         =>
            value match
                case Value.ObjectWithValues(values) => ValidationResult.summarize(values.values.map(v => validateValue(valueSchema, v)))
                case value                          => ValidationResult.reportError(s"expected map/object value; ${ value.valueTypeDescription } found")
        case NamedValueReference(_)        => ValidationResult.reportError(s"#WTF: unresolved NamedValueReference still present")
        case ImportStatement(_, _)         => ValidationResult.reportError(s"#WTF: unresolved ImportStatement still present")
        case ScopedReference(_, _)         => ValidationResult.reportError(s"#WTF: unresolved ScopedReference still present")
