package rengbis

import zio.Chunk
import zio.parser.{ AnySyntaxOps, Parser, ParserOps, Printer, StringErrSyntaxOps, StringParserError, Syntax, SyntaxOps }
import Schema.{ AlternativeValues, BinaryValue, Deprecated, Documented, EnumValues, ImportStatement, MandatoryLabel, NamedValueReference, NumericValue, ObjectLabel, OptionalLabel, Schema, ScopedReference, TextValue, TimeValue }
import Schema.{ BinaryConstraint, BoundConstraint, BoundOp, GivenTextValue, ListConstraint, NumericConstraint, TextConstraint, TimeConstraint }

object SchemaSyntax:
    import Extensions.*
    import BoundConstraintHelpers.boundConstraintSyntax

    type SchemaSyntax[A] = zio.parser.Syntax[String, Char, Char, A]

    val Tab   = '\u0009'
    val Space = '\u0020'
    val LF    = '\u000A'
    val CR    = '\u000D'

    val whitespaces: SchemaSyntax[Unit]  = Syntax.charIn(' ', '\t', '\r').*.unit(Chunk(' '))
    val whitespaces0: SchemaSyntax[Unit] = Syntax.charIn(' ', '\t', '\r').*.unit(Chunk())
    // Regular comment: # not followed by # (to distinguish from ## doc comments)
    val comment: SchemaSyntax[Unit]      =
        (Syntax.char('#') ~> Syntax.charNotIn('#') ~ Syntax.charNotIn('\n').repeat0).unit((' ', Chunk.empty))
            <> Syntax.char('#').unit(())

    // Documentation comments: ## at start of line
    val docCommentLine: SchemaSyntax[String]              = (
        whitespaces0 ~ Syntax.string("##", ()) ~ whitespaces ~> Syntax.charNotIn('\n').repeat0.string <~ Syntax.char('\n')
    )
    val precedingDocComment: SchemaSyntax[Option[String]] = docCommentLine.repeat0.transform(
        lines => if lines.isEmpty then None else Some(lines.map(_.trim).mkString("\n")),
        doc => doc.map(s => Chunk.fromIterable(s.split("\n").toSeq)).getOrElse(Chunk.empty)
    )

    // Trailing doc comment: ## at end of line (single line only)
    val trailingDocComment: SchemaSyntax[Option[String]] = (
        whitespaces ~ Syntax.string("##", ()) ~ whitespaces0 ~> Syntax.charNotIn('\n').repeat0.string
    ).optional.transform(
        opt => opt.map(_.trim).filter(_.nonEmpty),
        doc => doc
    )

    val deprecatedMarker: SchemaSyntax[Boolean] = (Syntax.string("@deprecated", ()) ~ whitespaces).optional
        .transform(
            opt => opt.isDefined,
            isDeprecated => if isDeprecated then Some(()) else None
        )

    val number: SchemaSyntax[Int] = Syntax.digit.repeat.transform(
        chars => chars.mkString.toInt,
        num => Chunk.fromArray(num.toString.toCharArray)
    )

    val emptyLines: SchemaSyntax[Unit] = (whitespaces ~ comment.optional ~ Syntax.char('\n')).repeat0.unit(Chunk())
    val newline: SchemaSyntax[Unit]    = Syntax.char('\n').unit(())

    val quote: SchemaSyntax[Unit]          = Syntax.char('\"')
    val escapedChar: SchemaSyntax[Char]    = Syntax.charNotIn('\"') // TODO: real escaping support
    val quotedString: SchemaSyntax[String] = (quote ~> escapedChar.*.string <~ quote)

    // Size constraint: parses/prints length bounds (e.g., "length >= 5", "5 <= length <= 10")
    val sizeTextConstraint: SchemaSyntax[TextConstraint.SizeRange] = boundConstraintSyntax(Syntax.string("length", ()), number)
        .transform(
            bounds => TextConstraint.SizeRange.fromBounds(bounds.map(_._2)),
            sizeRange =>
                val bounds = Chunk.fromIterable(sizeRange.min) ++ Chunk.fromIterable(sizeRange.max)
                bounds.map(b => ((), b))
        )

    // Regex constraint: parses/prints regex = "pattern"
    val regexTextConstraint: SchemaSyntax[String] = (
        Syntax.string("regex", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    )

    // Format constraint: parses/prints pattern = "format"
    val formatTextConstraint: SchemaSyntax[String] = (
        Syntax.string("pattern", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    )

    // A single text constraint element (used for parsing in any order)
    sealed trait TextConstraintElement
    case class SizeElement(size: TextConstraint.SizeRange)  extends TextConstraintElement
    case class RegexElement(regex: String)                  extends TextConstraintElement
    case class FormatElement(format: String)                extends TextConstraintElement

    val textConstraintElement: SchemaSyntax[TextConstraintElement] =
        sizeTextConstraint.transform(SizeElement(_), _.size).as[TextConstraintElement] { case s: SizeElement => s }
            <> regexTextConstraint.transform(RegexElement(_), _.regex).as[TextConstraintElement] { case r: RegexElement => r }
            <> formatTextConstraint.transform(FormatElement(_), _.format).as[TextConstraintElement] { case f: FormatElement => f }

    val textConstraints: SchemaSyntax[TextConstraint.Constraints] =
        textConstraintElement.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces).transform(
            elements =>
                val size   = elements.collectFirst { case SizeElement(s) => s }
                val regex  = elements.collectFirst { case RegexElement(r) => r }
                val format = elements.collectFirst { case FormatElement(f) => f }
                TextConstraint.Constraints(size, regex, format),
            constraints =>
                val sizeChunk   = Chunk.fromIterable(constraints.size.map(SizeElement(_)))
                val regexChunk  = Chunk.fromIterable(constraints.regex.map(RegexElement(_)))
                val formatChunk = Chunk.fromIterable(constraints.format.map(FormatElement(_)))
                sizeChunk ++ regexChunk ++ formatChunk
        )

    val textConstraintBlock: SchemaSyntax[TextConstraint.Constraints] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> textConstraints <~ whitespaces ~ Syntax.char(']')
    )

    val textDefaultValue: SchemaSyntax[String] = whitespaces ~ Syntax.string("?=", ()) ~ whitespaces ~> quotedString

    val textValue: SchemaSyntax[Schema.TextValue] = (
        Syntax.string("text", ()) ~ textConstraintBlock.optional ~ textDefaultValue.optional
    ).transform(
        { case (constraints, default) => TextValue(constraints.getOrElse(TextConstraint.Constraints.empty), default) },
        textValue =>
            (
                if textValue.constraints.isEmpty then None else Some(textValue.constraints),
                textValue.default
            )
    )

    val anyValue: SchemaSyntax[Schema.AnyValue] = Syntax.string("any", Schema.AnyValue())

    val booleanDefaultValue: SchemaSyntax[Boolean] = whitespaces ~ Syntax.string("?=", ()) ~ whitespaces ~> (
        Syntax.string("true", true) <> Syntax.string("false", false)
    )

    val booleanValue: SchemaSyntax[Schema.BooleanValue] = (
        Syntax.string("boolean", ()) ~ booleanDefaultValue.optional
    ).transform(
        { case default => Schema.BooleanValue(default) },
        bv => bv.default
    )
    val givenTextValue: SchemaSyntax[Schema.GivenTextValue] = quotedString.transform(s => Schema.GivenTextValue(s), v => v.value)

    val decimalNumber: SchemaSyntax[BigDecimal] = (
        Syntax.charIn('-').optional ~ Syntax.digit.repeat ~ (Syntax.char('.') ~ Syntax.digit.repeat).optional
    ).string.transform(
        str => BigDecimal(str),
        num => num.toString
    )

    // Numeric constraint: parses/prints value bounds (e.g., "value >= 0", "0 <= value <= 100")
    val valueNumericConstraint: SchemaSyntax[NumericConstraint.ValueRange] = boundConstraintSyntax(Syntax.string("value", ()), decimalNumber)
        .transform(
            bounds => NumericConstraint.ValueRange.fromBounds(bounds.map(_._2)),
            valueRange =>
                val bounds = Chunk.fromIterable(valueRange.min) ++ Chunk.fromIterable(valueRange.max)
                bounds.map(b => ((), b))
        )

    val integerConstraint: SchemaSyntax[Boolean] = Syntax.string("integer", true)

    // Numeric constraint elements (used for parsing in any order)
    sealed trait NumericConstraintElement
    case class ValueElement(value: NumericConstraint.ValueRange) extends NumericConstraintElement
    case class IntegerElement(integer: Boolean)                  extends NumericConstraintElement

    val numericConstraintElement: SchemaSyntax[NumericConstraintElement] =
        valueNumericConstraint.transform(ValueElement(_), _.value).as[NumericConstraintElement] { case v: ValueElement => v }
            <> integerConstraint.transform(IntegerElement(_), _.integer).as[NumericConstraintElement] { case i: IntegerElement => i }

    val numericConstraints: SchemaSyntax[NumericConstraint.Constraints] =
        numericConstraintElement.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces).transform(
            elements =>
                val value   = elements.collectFirst { case ValueElement(v) => v }
                val integer = elements.exists { case IntegerElement(true) => true; case _ => false }
                NumericConstraint.Constraints(value, integer),
            constraints =>
                val valueChunk   = Chunk.fromIterable(constraints.value.map(ValueElement(_)))
                val integerChunk = if constraints.integer then Chunk(IntegerElement(true)) else Chunk.empty
                valueChunk ++ integerChunk
        )

    val numericConstraintBlock: SchemaSyntax[NumericConstraint.Constraints] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> numericConstraints <~ whitespaces ~ Syntax.char(']')
    )

    val numericDefaultValue: SchemaSyntax[BigDecimal] = whitespaces ~ Syntax.string("?=", ()) ~ whitespaces ~> decimalNumber

    val numericValue: SchemaSyntax[Schema.NumericValue] = (
        Syntax.string("number", ()) ~ numericConstraintBlock.optional ~ numericDefaultValue.optional
    ).transform(
        { case (constraints, default) => NumericValue(constraints.getOrElse(NumericConstraint.Constraints.empty), default) },
        numericValue =>
            (
                if numericValue.constraints.isEmpty then None else Some(numericValue.constraints),
                numericValue.default
            )
    )

    val binaryToTextEncoder: SchemaSyntax[BinaryConstraint.BinaryToTextEncoder] =
        Syntax.string("'hex'", BinaryConstraint.BinaryToTextEncoder.hex)
            <> Syntax.string("'base64'", BinaryConstraint.BinaryToTextEncoder.base64)
            <> Syntax.string("'base32'", BinaryConstraint.BinaryToTextEncoder.base32)
            <> Syntax.string("'base58'", BinaryConstraint.BinaryToTextEncoder.base58)
            <> Syntax.string("'ascii85'", BinaryConstraint.BinaryToTextEncoder.ascii85)

    val encodingBinaryConstraint: SchemaSyntax[BinaryConstraint.BinaryToTextEncoder] = (
        Syntax.string("encoding", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> binaryToTextEncoder
    )

    val sizeBinaryUnit: SchemaSyntax[BinaryConstraint.BinaryUnit] =
        Syntax.string("bytes", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("bits", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("KB", BinaryConstraint.BinaryUnit.KB)
            <> Syntax.string("MB", BinaryConstraint.BinaryUnit.MB)
            <> Syntax.string("GB", BinaryConstraint.BinaryUnit.GB)

    // Binary size constraint: parses/prints size bounds (values are normalized to bytes)
    val sizeBinaryConstraint: SchemaSyntax[BinaryConstraint.SizeRange] = boundConstraintSyntax(sizeBinaryUnit, number)
        .transform(
            bounds =>
                val normalizedBounds = bounds.map((unit, b) => BoundConstraint(b.op, b.value * unit.bytes))
                BinaryConstraint.SizeRange.fromBounds(normalizedBounds),
            sizeRange =>
                val bounds = Chunk.fromIterable(sizeRange.min) ++ Chunk.fromIterable(sizeRange.max)
                bounds.map(b => (BinaryConstraint.BinaryUnit.bytes, b))
        )

    // Binary constraint elements (used for parsing in any order)
    sealed trait BinaryConstraintElement
    case class BinarySizeElement(size: BinaryConstraint.SizeRange)            extends BinaryConstraintElement
    case class EncodingElement(encoding: BinaryConstraint.BinaryToTextEncoder) extends BinaryConstraintElement

    val binaryConstraintElement: SchemaSyntax[BinaryConstraintElement] =
        sizeBinaryConstraint.transform(BinarySizeElement(_), _.size).as[BinaryConstraintElement] { case s: BinarySizeElement => s }
            <> encodingBinaryConstraint.transform(EncodingElement(_), _.encoding).as[BinaryConstraintElement] { case e: EncodingElement => e }

    val binaryConstraints: SchemaSyntax[BinaryConstraint.Constraints] =
        binaryConstraintElement.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces).transform(
            elements =>
                val size     = elements.collectFirst { case BinarySizeElement(s) => s }
                val encoding = elements.collectFirst { case EncodingElement(e) => e }
                BinaryConstraint.Constraints(size, encoding),
            constraints =>
                val sizeChunk     = Chunk.fromIterable(constraints.size.map(BinarySizeElement(_)))
                val encodingChunk = Chunk.fromIterable(constraints.encoding.map(EncodingElement(_)))
                sizeChunk ++ encodingChunk
        )

    val binaryConstraintBlock: SchemaSyntax[BinaryConstraint.Constraints] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> binaryConstraints <~ whitespaces ~ Syntax.char(']')
    )

    val binaryValue: SchemaSyntax[Schema.BinaryValue] = (
        Syntax.string("binary", ()) ~ binaryConstraintBlock.optional
    ).transform(
        constraints => BinaryValue(constraints.getOrElse(BinaryConstraint.Constraints.empty)),
        binaryValue => if binaryValue.constraints.isEmpty then None else Some(binaryValue.constraints)
    )

    val namedTimeFormat: SchemaSyntax[TimeConstraint.NamedFormat] =
        Syntax.string("'iso8601'", TimeConstraint.NamedFormat.ISO8601)
            <> Syntax.string("'iso8601-datetime'", TimeConstraint.NamedFormat.ISO8601_DateTime)
            <> Syntax.string("'iso8601-date'", TimeConstraint.NamedFormat.ISO8601_Date)
            <> Syntax.string("'iso8601-time'", TimeConstraint.NamedFormat.ISO8601_Time)
            <> Syntax.string("'rfc3339'", TimeConstraint.NamedFormat.RFC3339)

    val customTimePattern: SchemaSyntax[TimeConstraint.CustomPattern] = quotedString.transformEither(
        pattern =>
            try Right(TimeConstraint.CustomPattern(pattern))
            catch case e: IllegalArgumentException => Left(s"Invalid time pattern '$pattern': ${ e.getMessage }"),

        c => Right(c.pattern)
    )

    val timeFormatConstraint: SchemaSyntax[TimeConstraint.Constraint] =
        Syntax.string("format", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> (
            namedTimeFormat.as[TimeConstraint.Constraint] { case f: TimeConstraint.NamedFormat => f }
                <> customTimePattern.as[TimeConstraint.Constraint] { case p: TimeConstraint.CustomPattern => p }
        )

    val timeConstraints: SchemaSyntax[Chunk[TimeConstraint.Constraint]] =
        timeFormatConstraint.+

    val timeConstraintBlock: SchemaSyntax[Chunk[TimeConstraint.Constraint]] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> timeConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')
    ).transform(_.flatten, Chunk(_))

    val timeValue: SchemaSyntax[Schema.TimeValue] = (
        Syntax.string("time", ()) ~ timeConstraintBlock.optional
    ).transform(
        constraints => TimeValue(constraints.getOrElse(Chunk()).toList*),
        timeValue => if timeValue.constraints.isEmpty then None else Some(Chunk.fromIterable(timeValue.constraints))
    )

    val label: SchemaSyntax[String] = (Syntax.letter ~ (Syntax.alphaNumeric <> Syntax.charIn("_-")).repeat0).string

    val madatoryLabel: SchemaSyntax[MandatoryLabel] = label.transform(
        s => MandatoryLabel(s),
        l => l.label
    )
    val optionalLabel: SchemaSyntax[OptionalLabel]  = (
        label <~ Syntax.char('?')
    ).transform(
        s => OptionalLabel(s),
        l => l.label
    )
    val objectLabel: SchemaSyntax[ObjectLabel]      =
        optionalLabel.as[ObjectLabel] { case o: OptionalLabel => o }
            <> madatoryLabel.as[ObjectLabel] { case m: MandatoryLabel => m }

    val valueDefinition: SchemaSyntax[Schema] = Syntax.char('=') ~ whitespaces ~ items

    val namedValue: SchemaSyntax[(String, Schema)] = (
        precedingDocComment ~ whitespaces0 ~ deprecatedMarker ~ label ~ whitespaces ~ valueDefinition ~ trailingDocComment
    ).transform(
        { case (precedingDoc, isDeprecated, name, schema, trailingDoc) =>
            val doc          = Documentation.combineDoc(precedingDoc, trailingDoc)
            val withDoc      = schema.withDoc(doc)
            val withMetadata = if isDeprecated then withDoc.asDeprecated else withDoc
            (name, withMetadata)
        },
        { case (name, schema) =>
            def unwrap(s: Schema): (Option[String], Boolean, Schema) = s match
                case Deprecated(inner)      => val (doc, _, s) = unwrap(inner); (doc, true, s)
                case Documented(doc, inner) => (doc, false, inner)
                case _                      => (None, false, s)
            val (doc, isDeprecated, inner)                           = unwrap(schema)
            (doc, isDeprecated, name, inner, None)
        }
    )

    val unquotedPath: SchemaSyntax[String] = Syntax.charNotIn(' ', '\t', '\r', '\n').repeat.string
    val importPath: SchemaSyntax[String]   = quotedString <> unquotedPath

    val importStatement: SchemaSyntax[(String, Schema)] = (
        label ~ whitespaces ~ Syntax.string("=>", ()) ~ whitespaces ~ Syntax.string("import", ()) ~ whitespaces ~ importPath
    ).transform(
        (namespace, path) => (namespace, ImportStatement(namespace, path)),
        (namespace, schema) =>
            schema match
                case ImportStatement(ns, path) => (ns, path)
                case _                         => (namespace, "")
    )

    val scopedReference: SchemaSyntax[Schema.ScopedReference] = (
        label ~ Syntax.char('.') ~ label
    ).transform(
        (namespace, name) => ScopedReference(namespace, name),
        ref => (ref.namespace, ref.name)
    )

    val namedValueReference: SchemaSyntax[Schema.NamedValueReference] = (
        (Syntax.letter ~ (Syntax.alphaNumeric <> Syntax.charIn("_-")).repeat0).string
    ).transform(
        label => NamedValueReference(label),
        schema => schema.reference
    )

    val objectField: SchemaSyntax[(ObjectLabel, Schema)] = (
        precedingDocComment ~ whitespaces0 ~ deprecatedMarker ~ objectLabel ~ Syntax.char(':') ~ whitespaces ~ items ~ trailingDocComment
    ).transform(
        { case (precedingDoc, isDeprecated, label, schema, trailingDoc) =>
            val doc          = Documentation.combineDoc(precedingDoc, trailingDoc)
            val withDoc      = schema.withDoc(doc)
            val withMetadata = if isDeprecated then withDoc.asDeprecated else withDoc
            (label, withMetadata)
        },
        { case (label, schema) =>
            def unwrap(s: Schema): (Option[String], Boolean, Schema) = s match
                case Deprecated(inner)      => val (doc, _, s) = unwrap(inner); (doc, true, s)
                case Documented(doc, inner) => (doc, false, inner)
                case _                      => (None, false, s)
            val (doc, isDeprecated, inner)                           = unwrap(schema)
            (doc, isDeprecated, label, inner, None)
        }
    )

    val objectValue: SchemaSyntax[Schema] = (
        Syntax.char('{') ~> trailingDocComment ~ (emptyLines ~ whitespaces ~ objectField).repeatWithSep(whitespaces0 ~ (Syntax.char(',') <> emptyLines))
            ~ whitespaces ~ emptyLines <~ whitespaces0 ~ Syntax.char('}')
    ).transform(
        { case (doc, keys) => Schema.ObjectValue(keys.toMap).withDoc(doc) },
        {
            case Documented(doc, obj: Schema.ObjectValue) => (doc, Chunk.fromIterable(obj.obj))
            case obj: Schema.ObjectValue                  => (None, Chunk.fromIterable(obj.obj))
            case _                                        => (None, Chunk.empty) // Should never happen
        }
    )

    val mapSpreadKey: SchemaSyntax[Unit]        = Syntax.string("...", ()) <> Syntax.string("…", ())
    val mapValue: SchemaSyntax[Schema.MapValue] = (
        Syntax.char('{') ~> whitespaces
            ~ mapSpreadKey ~ whitespaces0 ~ Syntax.char(':') ~ whitespaces ~ items
            ~ whitespaces <~ Syntax.char('}')
    ).transform(
        valueSchema => Schema.MapValue(valueSchema),
        mapVal => mapVal.valueSchema
    )

    val item: SchemaSyntax[Schema] =
        anyValue.asSchema { case a: Schema.AnyValue => a }
            <> booleanValue.asSchema { case b: Schema.BooleanValue => b }
            <> numericValue.asSchema { case n: Schema.NumericValue => n }
            <> binaryValue.asSchema { case b: Schema.BinaryValue => b }
            <> timeValue.asSchema { case d: Schema.TimeValue => d }
            <> textValue.asSchema { case t: Schema.TextValue => t }
            <> givenTextValue.asSchema { case g: Schema.GivenTextValue => g }
            <> mapValue.asSchema { case m: Schema.MapValue => m }
            <> objectValue.asSchema { case o: Schema.ObjectValue => o; case d @ Documented(_, _: Schema.ObjectValue) => d }
            <> groupedAlternatives.asSchema { case e: Schema.EnumValues => e; case a: Schema.AlternativeValues => a }
            <> tupleValues.asSchema { case t: Schema.TupleValue => t }
            <> scopedReference.asSchema { case s: Schema.ScopedReference => s }
            <> namedValueReference.asSchema { case n: Schema.NamedValueReference => n }

    // List size constraint: parses/prints size bounds (e.g., "size >= 1", "1 <= size <= 10")
    val listSizeConstraint: SchemaSyntax[ListConstraint.SizeRange] = boundConstraintSyntax(Syntax.string("size", ()), number)
        .transform(
            bounds => ListConstraint.SizeRange.fromBounds(bounds.map(_._2)),
            sizeRange =>
                val bounds = Chunk.fromIterable(sizeRange.min) ++ Chunk.fromIterable(sizeRange.max)
                bounds.map(b => ((), b))
        )

    // Uniqueness constraints
    val uniqueSimpleConstraint: SchemaSyntax[ListConstraint.Uniqueness] =
        Syntax.string("unique", ListConstraint.Uniqueness.Simple)

    // Parses: unique = (field1, field2, ...)
    val uniqueByFieldsSyntax: SchemaSyntax[Seq[String]] = (
        Syntax.string("unique", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~
            Syntax.char('(') ~ whitespaces ~
            label.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) ~
            whitespaces ~ Syntax.char(')')
    ).transform(
        fields => fields.toSeq,
        fields => Chunk.fromIterable(fields)
    )

    // Parses: unique = field
    val uniqueByFieldSyntax: SchemaSyntax[String] = (
        Syntax.string("unique", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> label
    )

    val uniquenessConstraint: SchemaSyntax[ListConstraint.Uniqueness] =
        uniqueByFieldsSyntax.transform(fields => ListConstraint.Uniqueness.ByFields(fields), _.asInstanceOf[ListConstraint.Uniqueness.ByFields].fields)
            .as[ListConstraint.Uniqueness] { case u @ ListConstraint.Uniqueness.ByFields(f) if f.size > 1 => u }
            <> uniqueByFieldSyntax.transform(field => ListConstraint.Uniqueness.ByFields(Seq(field)), _.asInstanceOf[ListConstraint.Uniqueness.ByFields].fields.head)
                .as[ListConstraint.Uniqueness] { case u @ ListConstraint.Uniqueness.ByFields(f) if f.size == 1 => u }
            <> uniqueSimpleConstraint.as[ListConstraint.Uniqueness] { case ListConstraint.Uniqueness.Simple => ListConstraint.Uniqueness.Simple }

    // List constraint elements (used for parsing in any order)
    sealed trait ListConstraintElement
    case class ListSizeElement(size: ListConstraint.SizeRange)       extends ListConstraintElement
    case class UniqueElement(unique: ListConstraint.Uniqueness)       extends ListConstraintElement

    val listConstraintElement: SchemaSyntax[ListConstraintElement] =
        listSizeConstraint.transform(ListSizeElement(_), _.size).as[ListConstraintElement] { case s: ListSizeElement => s }
            <> uniquenessConstraint.transform(UniqueElement(_), _.unique).as[ListConstraintElement] { case u: UniqueElement => u }

    val listConstraints: SchemaSyntax[ListConstraint.Constraints] =
        listConstraintElement.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces).transform(
            elements =>
                val size = elements.collectFirst { case ListSizeElement(s) => s }
                // Collect all unique constraints as independent constraints
                // Each `unique = field` becomes a separate ByFields(Seq(field))
                // `unique = (field1, field2)` becomes a single ByFields(Seq(field1, field2)) (composite key)
                // `unique` becomes Simple
                val uniqueElements = elements.collect { case UniqueElement(u) => u }.toSeq
                // Merge multiple Simple into one, but keep ByFields separate for independent validation
                val unique = if uniqueElements.contains(ListConstraint.Uniqueness.Simple) then
                    Seq(ListConstraint.Uniqueness.Simple) ++ uniqueElements.filterNot(_ == ListConstraint.Uniqueness.Simple)
                else
                    uniqueElements
                ListConstraint.Constraints(size, unique),
            constraints =>
                val sizeChunk = Chunk.fromIterable(constraints.size.map(ListSizeElement(_)))
                // Each uniqueness constraint becomes a separate element
                val uniqueChunk = Chunk.fromIterable(constraints.unique.map(UniqueElement(_)))
                sizeChunk ++ uniqueChunk
        )

    val listConstraintBlock: SchemaSyntax[ListConstraint.Constraints] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~>
            listConstraints
            <~ whitespaces ~ Syntax.char(']')
    )

    val nonEmptyMarker: SchemaSyntax[Boolean] = Syntax.string("+", true) <> Syntax.string("*", false)

    val listSuffix: SchemaSyntax[ListConstraint.Constraints] =
        val atLeastOne = ListConstraint.SizeRange.minInclusive(1)

        (nonEmptyMarker ~ listConstraintBlock.optional).transform(
            (isPlus, extra) =>
                val baseSize = if isPlus then Some(atLeastOne) else None
                val extraConstraints = extra.getOrElse(ListConstraint.Constraints.empty)
                // Merge: if we have both + marker and explicit size, combine them
                // The + marker always enforces min >= 1, merged with any explicit size constraint
                val mergedSize = (baseSize, extraConstraints.size) match
                    case (Some(base), Some(explicit)) => Some(base.merge(explicit))
                    case (Some(base), None)           => Some(base)
                    case (None, Some(explicit))       => Some(explicit)
                    case (None, None)                 => None
                ListConstraint.Constraints(
                    size = mergedSize,
                    unique = extraConstraints.unique
                )
            ,
            constraints =>
                val hasMinOne = constraints.size.exists(s => s.min.exists(b => b.isMinInclusive && b.value == 1) && s.max.isEmpty)
                val others    = if hasMinOne then constraints.copy(size = None) else constraints
                (hasMinOne, if others.isEmpty then None else Some(others))
        )

    val listOfValues: SchemaSyntax[Schema.ListOfValues] =
        (item ~ listSuffix).transform(
            (schema, constraints) => Schema.ListOfValues(schema, constraints),
            list => (list.schema, list.constraints)
        )

    val tupleValues: SchemaSyntax[Schema.TupleValue] = (Syntax.char('(') ~> (emptyLines ~ whitespaces0 ~ item).repeatWithSep(whitespaces0 ~ (Syntax.char(',') <> emptyLines) ~ whitespaces) <~ whitespaces0 ~ Syntax.char(')'))
        .filter(values => values.size > 1, s"tuple needs to have at least two items")
        .transform(
            values => Schema.TupleValue(values*),
            tuple => Chunk.fromIterable(tuple.options)
        )

    val alternativeValuesOptions: SchemaSyntax[Schema] =
        listOfValues.asSchema { case l: Schema.ListOfValues => l } <> item

    val alternativeValues: SchemaSyntax[Schema.AlternativeValues | Schema.EnumValues] = (alternativeValuesOptions
        .repeatWithSep(whitespaces0 ~ emptyLines ~ whitespaces ~ Syntax.char('|') ~ whitespaces))
        .filter(options => options.size > 1, s"alternatives needs to have at least two items")
        .transform(
            options =>
                if (options.forall(s => s match { case GivenTextValue(_) => true; case _ => false }))
                then Schema.EnumValues(options.map(s => s match { case GivenTextValue(value) => value; case _ => "" })*)
                else Schema.AlternativeValues(options*),
            alternatives =>
                alternatives match
                    case EnumValues(values*)         => Chunk.fromIterable(values.map(v => Schema.GivenTextValue(v)))
                    case AlternativeValues(options*) => Chunk.fromIterable(options)
        )

    val groupedAlternatives: SchemaSyntax[Schema.AlternativeValues | Schema.EnumValues] =
        Syntax.char('(') ~ whitespaces ~> alternativeValues <~ whitespaces ~ Syntax.char(')')

    lazy val items: SchemaSyntax[Schema] =
        alternativeValues.asSchema { case e: Schema.EnumValues => e; case a: Schema.AlternativeValues => a }
            <> listOfValues.asSchema { case l: Schema.ListOfValues => l }
            <> tupleValues.asSchema { case t: Schema.TupleValue => t }
            <> item

    // Root schema with optional doc comments (preceding ## lines and/or trailing ##)
    val root: SchemaSyntax[Schema] = (
        precedingDocComment ~ whitespaces0 ~ valueDefinition ~ trailingDocComment
    ).transform(
        { case (precedingDoc, schema, trailingDoc) =>
            val doc = Documentation.combineDoc(precedingDoc, trailingDoc)
            schema.withDoc(doc)
        },
        schema =>
            schema match
                case Documented(doc, inner) => (doc, inner, None)
                case _                      => (None, schema, None)
    )

    val definition: SchemaSyntax[(String, Schema)] = importStatement <> namedValue

    val schema: SchemaSyntax[(Chunk[(String, Schema)], Option[Schema])] = (
        (emptyLines
            ~ definition.repeatWithSep0(emptyLines)
            ~ emptyLines
            ~ root.optional
            ~ emptyLines ~ whitespaces) <~ Syntax.end
    )

    // ========================================================================

    object Documentation:
        /** Combines preceding and trailing doc comments into a single Option[String] */
        def combineDoc(preceding: Option[String], trailing: Option[String]): Option[String] =
            (preceding, trailing) match
                case (Some(p), Some(t)) => Some(s"$p\n$t")
                case (Some(p), None)    => Some(p)
                case (None, Some(t))    => Some(t)
                case (None, None)       => None

    object BoundConstraintHelpers:
        def boundConstraintSyntax[K, V](keySyntax: SchemaSyntax[K], valueSyntax: SchemaSyntax[V]): SchemaSyntax[Chunk[(K, BoundConstraint[V])]] =
            // Form 1: value op key op value (e.g., "5 <= length <= 10") - must come first for printing ranges
            (valueSyntax ~ whitespaces ~ smallerOpAsMin ~ whitespaces ~ keySyntax ~ whitespaces ~ smallerOpAsMax ~ whitespaces ~ valueSyntax)
                .transformTo(
                    { case (lo, minOp, k, maxOp, up) => Chunk((k, BoundConstraint(minOp, lo)), (k, BoundConstraint(maxOp, up))) },
                    bracketed,
                    "not a range constraint"
                )
            // Form 2: key op value (e.g., "length >= 5")
                <> (keySyntax ~ whitespaces ~ boundOpForKeyOpValue ~ whitespaces ~ valueSyntax).transformTo(
                    { case (k, op, v) => Chunk((k, BoundConstraint(op, v))) },
                    rightValue,
                    "not a single bound constraint"
                )
                // Form 3: value op key (e.g., "5 <= length")
                <> (valueSyntax ~ whitespaces ~ smallerOpAsMin ~ whitespaces ~ keySyntax).transformTo(
                    { case (v, op, k) => Chunk((k, BoundConstraint(op, v))) },
                    leftValue,
                    "not a min bound constraint"
                )

        protected val boundOpForKeyOpValue: SchemaSyntax[BoundOp] =
            Syntax.string("==", BoundOp.Exact)
                <> Syntax.string(">=", BoundOp.MinInclusive)
                <> Syntax.string(">", BoundOp.MinExclusive)
                <> Syntax.string("<=", BoundOp.MaxInclusive)
                <> Syntax.string("<", BoundOp.MaxExclusive)

        protected val smallerOpAsMin: SchemaSyntax[BoundOp] = Syntax.string("<=", BoundOp.MinInclusive) <> Syntax.string("<", BoundOp.MinExclusive)
        protected val smallerOpAsMax: SchemaSyntax[BoundOp] = Syntax.string("<=", BoundOp.MaxInclusive) <> Syntax.string("<", BoundOp.MaxExclusive)

        protected def isMinBound(op: BoundOp): Boolean = op == BoundOp.MinInclusive || op == BoundOp.MinExclusive
        protected def isMaxBound(op: BoundOp): Boolean = op == BoundOp.MaxInclusive || op == BoundOp.MaxExclusive

        /** Prints "value op key op value" (e.g., "5 <= length <= 10") */
        protected def bracketed[K, V]: PartialFunction[Chunk[(K, BoundConstraint[V])], (V, BoundOp, K, BoundOp, V)] =
            case c if c.size == 2 && c.exists((_, b) => isMinBound(b.op)) && c.exists((_, b) => isMaxBound(b.op)) =>
                val (k, minBound) = c.find((_, b) => isMinBound(b.op)).get
                val (_, maxBound) = c.find((_, b) => isMaxBound(b.op)).get
                (minBound.value, minBound.op, k, maxBound.op, maxBound.value)

        /** Prints "key op value" (e.g., "length >= 5") */
        protected def rightValue[K, V]: PartialFunction[Chunk[(K, BoundConstraint[V])], (K, BoundOp, V)] =
            case c if c.size == 1 =>
                val (k, bound) = c.head
                (k, bound.op, bound.value)

        /** Prints "value op key" (e.g., "5 <= length") */
        protected def leftValue[K, V]: PartialFunction[Chunk[(K, BoundConstraint[V])], (V, BoundOp, K)] =
            case c if c.size == 1 && isMinBound(c.head._2.op) =>
                val (k, bound) = c.head
                (bound.value, bound.op, k)

    // . .  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

    object Extensions:
        extension [A <: Schema](syntax: SchemaSyntax[A])
            def asSchema(narrow: PartialFunction[Schema, A]): SchemaSyntax[Schema] =
                syntax.transformTo[String, Schema, Schema](identity, narrow, "type mismatch")

        extension [A](syntax: SchemaSyntax[A])
            /** Widens A to B (where A <: B) using a partial function for the reverse (printing) direction */
            def as[B >: A](narrow: PartialFunction[B, A]): SchemaSyntax[B] =
                syntax.transformTo[String, B, B](identity, narrow, "type mismatch")

        extension [A](syntax: SchemaSyntax[Chunk[A]])
            /** Widens Chunk[A] to Chunk[B], checking all elements are of type A when printing */
            def asChunkOf[B >: A](narrow: PartialFunction[B, A]): SchemaSyntax[Chunk[B]] =
                syntax.transformTo[String, Chunk[B], Chunk[B]](
                    identity,
                    { case cs if cs.forall(narrow.isDefinedAt) => cs.map(narrow) },
                    "type mismatch"
                )
