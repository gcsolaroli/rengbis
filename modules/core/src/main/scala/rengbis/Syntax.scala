package rengbis

import zio.Chunk
import zio.parser.{ AnySyntaxOps, Parser, ParserOps, Printer, StringErrSyntaxOps, StringParserError, Syntax, SyntaxOps }
import Schema.{ AlternativeValues, BinaryValue, EnumValues, ImportStatement, MandatoryLabel, NamedValueReference, NumericValue, ObjectLabel, OptionalLabel, Schema, ScopedReference, TextValue }
import Schema.{ BinaryConstraint, GivenTextValue, ListConstraint, NumericConstraint, TextConstraint }

object SchemaSyntax:
    type SchemaSyntax[A] = zio.parser.Syntax[String, Char, Char, A]

    val Tab   = '\u0009'
    val Space = '\u0020'
    val LF    = '\u000A'
    val CR    = '\u000D'

    val whitespaces: SchemaSyntax[Unit] = Syntax.charIn(' ', '\t', '\r').*.unit(Chunk())
    val comment: SchemaSyntax[Unit]     = (Syntax.char('#') ~ Syntax.charNotIn('\n').repeat0).unit(Chunk())

    val number: SchemaSyntax[Int] = Syntax.digit.repeat.transform(
        chars => chars.mkString.toInt,
        num => Chunk.fromArray(num.toString.toCharArray)
    )

    val emptyLines: SchemaSyntax[Unit] = (whitespaces ~ comment.optional ~ Syntax.char('\n')).repeat0.unit(Chunk())

    val quote: SchemaSyntax[Unit]          = Syntax.char('\"')
    val escapedChar: SchemaSyntax[Char]    = Syntax.charNotIn('\"') // TODO: real escaping support
    val quotedString: SchemaSyntax[String] = (quote ~> escapedChar.*.string <~ quote)

    sealed trait SizeConstraint
    enum SmallerSizeConstraint  extends SizeConstraint:
        case LT, LE
    enum ExtendedSizeConstraint extends SizeConstraint:
        case EQ, GT, GE

    val smallerSizeConstrint: SchemaSyntax[SmallerSizeConstraint] =
        Syntax.string("<=", SmallerSizeConstraint.LE)
            <> Syntax.string("<", SmallerSizeConstraint.LT)

    val sizeConstraint: SchemaSyntax[SizeConstraint] =
        Syntax.string("==", ExtendedSizeConstraint.EQ)
            <> Syntax.string(">=", ExtendedSizeConstraint.GE)
            <> Syntax.string(">", ExtendedSizeConstraint.GT)
            <> smallerSizeConstrint.widen[SizeConstraint]

    val sizeTextConstraints: SchemaSyntax[Chunk[TextConstraint.TextSizeConstraint]] = (
        Syntax.string("length", ()) ~ whitespaces ~ sizeConstraint ~ whitespaces ~ number
    ).transform(
        (c, n) =>
            Chunk(
                c match
                    case ExtendedSizeConstraint.EQ => TextConstraint.Length(n)
                    case ExtendedSizeConstraint.GT => TextConstraint.MinLength(n + 1)
                    case ExtendedSizeConstraint.GE => TextConstraint.MinLength(n)
                    case SmallerSizeConstraint.LT  => TextConstraint.MaxLength(n - 1)
                    case SmallerSizeConstraint.LE  => TextConstraint.MaxLength(n)
            ),
        c =>
            c.head match
                case TextConstraint.Length(l)    => (ExtendedSizeConstraint.EQ, l)
                case TextConstraint.MinLength(l) => (ExtendedSizeConstraint.GE, l)
                case TextConstraint.MaxLength(l) => (SmallerSizeConstraint.LE, l)
    )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("length", ()) ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ number
        ).transform(
            (l, lc, uc, u) =>
                (lc, uc) match
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LT) => Chunk(TextConstraint.MinLength(l + 1), TextConstraint.MaxLength(u - 1))
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LE) => Chunk(TextConstraint.MinLength(l + 1), TextConstraint.MaxLength(u))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LT) => Chunk(TextConstraint.MinLength(l), TextConstraint.MaxLength(u - 1))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LE) => Chunk(TextConstraint.MinLength(l), TextConstraint.MaxLength(u))
            ,
            c => (c.head.size, SmallerSizeConstraint.LE, SmallerSizeConstraint.LE, c.tail.head.size)
        )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("length", ())
        ).transform(
            (l, lc) =>
                lc match
                    case SmallerSizeConstraint.LT => Chunk(TextConstraint.MinLength(l + 1))
                    case SmallerSizeConstraint.LE => Chunk(TextConstraint.MinLength(l))
            ,
            c => (c.head.size, SmallerSizeConstraint.LE)
        )

    val regexTextConstraint: SchemaSyntax[TextConstraint.Constraint] = (
        Syntax.string("regex", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    ).transform(
        text => TextConstraint.Regex(text),
        c => c.pattern
    ).widen[TextConstraint.Constraint]

    val formatTextConstraint: SchemaSyntax[TextConstraint.Constraint] = (
        Syntax.string("pattern", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    ).transform(
        text => TextConstraint.Format(text),
        c => c.format
    ).widen[TextConstraint.Constraint]

    val textConstraints: SchemaSyntax[Chunk[TextConstraint.Constraint]] =
        sizeTextConstraints.widen[Chunk[TextConstraint.Constraint]]
            <> regexTextConstraint.+
            <> formatTextConstraint.+

    val textValue: SchemaSyntax[Schema.TextValue] = (
        Syntax.string("text", ()) ~ (whitespaces ~ Syntax.char('[') ~ whitespaces ~> textConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')).optional
    ).transform(
        constraints => TextValue(constraints.map(_.flatMap(identity)).getOrElse(Chunk()).toList*),
        textValue => if textValue.constraints.isEmpty then None else Some(Chunk(Chunk.fromIterable(textValue.constraints)))
    )

    val anyValue: SchemaSyntax[Schema.AnyValue]             = Syntax.string("any", Schema.AnyValue())
    val booleanValue: SchemaSyntax[Schema.BooleanValue]     = Syntax.string("boolean", Schema.BooleanValue())
    val givenTextValue: SchemaSyntax[Schema.GivenTextValue] = quotedString.transform(s => Schema.GivenTextValue(s), v => v.value)

    val decimalNumber: SchemaSyntax[BigDecimal] = (
        Syntax.charIn('-').optional ~ Syntax.digit.repeat ~ (Syntax.char('.') ~ Syntax.digit.repeat).optional
    ).string.transform(
        str => BigDecimal(str),
        num => num.toString
    )

    val integerConstraint: SchemaSyntax[NumericConstraint.Constraint] = Syntax.string("integer", NumericConstraint.Integer).widen[NumericConstraint.Constraint]

    val valueNumericConstraints: SchemaSyntax[Chunk[NumericConstraint.NumericValueConstraint]] = (
        Syntax.string("value", ()) ~ whitespaces ~ sizeConstraint ~ whitespaces ~ decimalNumber
    ).transform(
        (c, n) =>
            Chunk(
                c match
                    case ExtendedSizeConstraint.EQ => NumericConstraint.ExactValue(n)
                    case ExtendedSizeConstraint.GT => NumericConstraint.MinValueExclusive(n)
                    case ExtendedSizeConstraint.GE => NumericConstraint.MinValue(n)
                    case SmallerSizeConstraint.LT  => NumericConstraint.MaxValueExclusive(n)
                    case SmallerSizeConstraint.LE  => NumericConstraint.MaxValue(n)
            ),
        c =>
            c.head match
                case NumericConstraint.ExactValue(v)        => (ExtendedSizeConstraint.EQ, v)
                case NumericConstraint.MinValueExclusive(v) => (ExtendedSizeConstraint.GT, v)
                case NumericConstraint.MinValue(v)          => (ExtendedSizeConstraint.GE, v)
                case NumericConstraint.MaxValueExclusive(v) => (SmallerSizeConstraint.LT, v)
                case NumericConstraint.MaxValue(v)          => (SmallerSizeConstraint.LE, v)
    )
        <> (
            decimalNumber ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("value", ()) ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ decimalNumber
        ).transform(
            (l, lc, uc, u) =>
                (lc, uc) match
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LT) => Chunk(NumericConstraint.MinValueExclusive(l), NumericConstraint.MaxValueExclusive(u))
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LE) => Chunk(NumericConstraint.MinValueExclusive(l), NumericConstraint.MaxValue(u))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LT) => Chunk(NumericConstraint.MinValue(l), NumericConstraint.MaxValueExclusive(u))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LE) => Chunk(NumericConstraint.MinValue(l), NumericConstraint.MaxValue(u))
            ,
            c =>
                val (minBound, minOp) = c.head match
                    case NumericConstraint.MinValueExclusive(v) => (v, SmallerSizeConstraint.LT)
                    case NumericConstraint.MinValue(v)          => (v, SmallerSizeConstraint.LE)
                    case other                                  => (other.bound, SmallerSizeConstraint.LE)
                val (maxBound, maxOp) = c.tail.head match
                    case NumericConstraint.MaxValueExclusive(v) => (v, SmallerSizeConstraint.LT)
                    case NumericConstraint.MaxValue(v)          => (v, SmallerSizeConstraint.LE)
                    case other                                  => (other.bound, SmallerSizeConstraint.LE)
                (minBound, minOp, maxOp, maxBound)
        )
        <> (
            decimalNumber ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("value", ())
        ).transform(
            (l, lc) =>
                lc match
                    case SmallerSizeConstraint.LT => Chunk(NumericConstraint.MinValueExclusive(l))
                    case SmallerSizeConstraint.LE => Chunk(NumericConstraint.MinValue(l))
            ,
            c =>
                c.head match
                    case NumericConstraint.MinValueExclusive(v) => (v, SmallerSizeConstraint.LT)
                    case NumericConstraint.MinValue(v)          => (v, SmallerSizeConstraint.LE)
                    case other                                  => (other.bound, SmallerSizeConstraint.LE)
        )

    val numericConstraints: SchemaSyntax[Chunk[NumericConstraint.Constraint]] =
        valueNumericConstraints.widen[Chunk[NumericConstraint.Constraint]]
            <> integerConstraint.+

    val numericValue: SchemaSyntax[Schema.NumericValue] = (
        Syntax.string("number", ()) ~ (whitespaces ~ Syntax.char('[') ~ whitespaces ~> numericConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')).optional
    ).transform(
        constraints => NumericValue(constraints.map(_.flatMap(identity)).getOrElse(Chunk()).toList*),
        numericValue => if numericValue.constraints.isEmpty then None else Some(Chunk(Chunk.fromIterable(numericValue.constraints)))
    )

    val binaryToTextEncoder: SchemaSyntax[BinaryConstraint.BinaryToTextEncoder] =
        Syntax.string("'hex'", BinaryConstraint.BinaryToTextEncoder.hex)
            <> Syntax.string("'base64'", BinaryConstraint.BinaryToTextEncoder.base64)
            <> Syntax.string("'base32'", BinaryConstraint.BinaryToTextEncoder.base32)
            <> Syntax.string("'base58'", BinaryConstraint.BinaryToTextEncoder.base58)
            <> Syntax.string("'ascii85'", BinaryConstraint.BinaryToTextEncoder.ascii85)

    val encodingConstraint: SchemaSyntax[BinaryConstraint.Constraint] = (
        Syntax.string("encoding", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> binaryToTextEncoder
    ).transform(
        encoder => BinaryConstraint.Encoding(encoder),
        c => c.encoder
    ).widen[BinaryConstraint.Constraint]

    val sizeBinaryUnit: SchemaSyntax[BinaryConstraint.BinaryUnit] =
        Syntax.string("bits", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("bytes", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("KB", BinaryConstraint.BinaryUnit.KB)
            <> Syntax.string("MB", BinaryConstraint.BinaryUnit.MB)
            <> Syntax.string("GB", BinaryConstraint.BinaryUnit.GB)

    val sizeBinaryConstraints: SchemaSyntax[Chunk[BinaryConstraint.SizeConstraint]] = (
        sizeBinaryUnit ~ whitespaces ~ sizeConstraint ~ whitespaces ~ number
    ).transform(
        (u, c, n) =>
            Chunk(
                c match
                    case ExtendedSizeConstraint.EQ => BinaryConstraint.ExactSize(n, u)
                    case ExtendedSizeConstraint.GT => BinaryConstraint.MinSize(n + 1, u)
                    case ExtendedSizeConstraint.GE => BinaryConstraint.MinSize(n, u)
                    case SmallerSizeConstraint.LT  => BinaryConstraint.MaxSize(n - 1, u)
                    case SmallerSizeConstraint.LE  => BinaryConstraint.MaxSize(n, u)
            ),
        c =>
            c.head match
                case BinaryConstraint.ExactSize(s, u) => (u, ExtendedSizeConstraint.EQ, s)
                case BinaryConstraint.MinSize(s, u)   => (u, ExtendedSizeConstraint.GE, s)
                case BinaryConstraint.MaxSize(s, u)   => (u, SmallerSizeConstraint.LE, s)
    )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ sizeBinaryUnit ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ number
        ).transform(
            (lo, lc, unit, uc, up) =>
                (lc, uc) match
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LT) => Chunk(BinaryConstraint.MinSize(lo + 1, unit), BinaryConstraint.MaxSize(up - 1, unit))
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LE) => Chunk(BinaryConstraint.MinSize(lo + 1, unit), BinaryConstraint.MaxSize(up, unit))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LT) => Chunk(BinaryConstraint.MinSize(lo, unit), BinaryConstraint.MaxSize(up - 1, unit))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LE) => Chunk(BinaryConstraint.MinSize(lo, unit), BinaryConstraint.MaxSize(up, unit))
            ,
            c =>
                val minSize = c.collectFirst { case BinaryConstraint.MinSize(s, u) => s }.getOrElse(0)
                val maxSize = c.collectFirst { case BinaryConstraint.MaxSize(s, u) => s }.getOrElse(0)
                val unit    = c.collectFirst { case BinaryConstraint.MaxSize(s, u) => u }.getOrElse(BinaryConstraint.BinaryUnit.bytes)
                (minSize, SmallerSizeConstraint.LE, unit, SmallerSizeConstraint.LE, maxSize)
        )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ sizeBinaryUnit
        ).transform(
            (l, lc, unit) =>
                lc match
                    case SmallerSizeConstraint.LT => Chunk(BinaryConstraint.MinSize(l + 1, unit))
                    case SmallerSizeConstraint.LE => Chunk(BinaryConstraint.MinSize(l, unit))
            ,
            c =>
                val (minSize, unit) = c.collectFirst { case BinaryConstraint.MinSize(s, u) => (s, u) }.getOrElse((0, BinaryConstraint.BinaryUnit.bytes))
                (minSize, SmallerSizeConstraint.LE, unit)
        )

    val binaryConstraints: SchemaSyntax[Chunk[BinaryConstraint.Constraint]] =
        sizeBinaryConstraints.widen[Chunk[BinaryConstraint.Constraint]]
            <> encodingConstraint.+

    val binaryValue: SchemaSyntax[Schema.BinaryValue] = (
        Syntax.string("binary", ()) ~ (whitespaces ~ Syntax.char('[') ~ whitespaces ~> binaryConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')).optional
    ).transform(
        constraints => BinaryValue(constraints.map(_.flatMap(identity)).getOrElse(Chunk()).toList*),
        binaryValue => if binaryValue.constraints.isEmpty then None else Some(Chunk(Chunk.fromIterable(binaryValue.constraints)))
    )

    val label: SchemaSyntax[String] = (Syntax.letter ~ (Syntax.alphaNumeric <> Syntax.charIn("_-")).repeat0).string

    val madatoryLabel: SchemaSyntax[ObjectLabel] = label.transform(
        s => MandatoryLabel(s),
        l => l.label
    )
    val optionalLabel: SchemaSyntax[ObjectLabel] = (
        label <~ Syntax.char('?')
    ).transform(
        s => OptionalLabel(s),
        l => l.label
    )
    val objectLabel: SchemaSyntax[ObjectLabel]   = optionalLabel <> madatoryLabel

    val valueDefinition: SchemaSyntax[Schema] = Syntax.char('=') ~ whitespaces ~ items

    val namedValue: SchemaSyntax[(String, Schema)] = label ~ whitespaces ~ valueDefinition

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

    val objectValue: SchemaSyntax[Schema.ObjectValue] = (
        Syntax.char('{') ~> (emptyLines ~ whitespaces ~
            objectLabel ~ Syntax.char(':') ~ whitespaces ~ items).repeatWithSep(whitespaces ~ (Syntax.char(',') <> emptyLines))
            ~ whitespaces ~ emptyLines <~ whitespaces ~ Syntax.char('}')
    ).transform(
        keys => Schema.ObjectValue(keys.toMap),
        obj => Chunk.fromIterable(obj.obj)
    )

    val mapSpreadKey: SchemaSyntax[Unit]        = Syntax.string("...", ()) <> Syntax.string("…", ())
    val mapValue: SchemaSyntax[Schema.MapValue] = (
        Syntax.char('{') ~> whitespaces
            ~ mapSpreadKey ~ whitespaces ~ Syntax.char(':') ~ whitespaces ~ items
            ~ whitespaces <~ Syntax.char('}')
    ).transform(
        valueSchema => Schema.MapValue(valueSchema),
        mapVal => mapVal.valueSchema
    )

    val item: SchemaSyntax[Schema] = anyValue.widen[Schema]
        <> booleanValue.widen[Schema]
        <> numericValue.widen[Schema]
        <> binaryValue.widen[Schema]
        <> textValue.widen[Schema]
        <> givenTextValue.widen[Schema]
        <> mapValue.widen[Schema]
        <> objectValue.widen[Schema]
        <> (Syntax.char('(') ~ whitespaces ~> alternativeValues <~ whitespaces ~ Syntax.char(')')).widen[Schema]
        <> tupleValues.widen[Schema]
        <> scopedReference.widen[Schema]
        <> namedValueReference.widen[Schema]

    val uniqueConstraint: SchemaSyntax[ListConstraint.Constraint]             = Syntax.string("unique", ListConstraint.Unique).widen[ListConstraint.Constraint]
    val uniqueByFieldsConstraint: SchemaSyntax[ListConstraint.UniqueByFields] = (
        Syntax.string("unique", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~
            Syntax.char('(') ~ whitespaces ~
            label.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) ~
            whitespaces ~ Syntax.char(')')
    ).transform(
        fields => ListConstraint.UniqueByFields(fields.toSeq),
        c => Chunk.fromIterable(c.fields)
    )
    val uniqueByFieldConstraint: SchemaSyntax[ListConstraint.UniqueByFields]  = (
        Syntax.string("unique", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~ label
    ).transform(
        fieldName => ListConstraint.UniqueByFields(Seq(fieldName)),
        c => c.fields.head
    )

    val listSizeConstraints: SchemaSyntax[Chunk[ListConstraint.Constraint]] = (
        Syntax.string("size", ()) ~ whitespaces ~ sizeConstraint ~ whitespaces ~ number
    ).transform(
        (c, n) =>
            Chunk(
                c match
                    case ExtendedSizeConstraint.EQ => ListConstraint.ExactSize(n)
                    case ExtendedSizeConstraint.GT => ListConstraint.MinSize(n + 1)
                    case ExtendedSizeConstraint.GE => ListConstraint.MinSize(n)
                    case SmallerSizeConstraint.LT  => ListConstraint.MaxSize(n - 1)
                    case SmallerSizeConstraint.LE  => ListConstraint.MaxSize(n)
            ),
        c =>
            c.head match
                case ListConstraint.ExactSize(s) => (ExtendedSizeConstraint.EQ, s)
                case ListConstraint.MinSize(s)   => (ExtendedSizeConstraint.GE, s)
                case ListConstraint.MaxSize(s)   => (SmallerSizeConstraint.LE, s)
                case _                           => (ExtendedSizeConstraint.EQ, 0)
    )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("size", ()) ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ number
        ).transform(
            (l, lc, uc, u) =>
                (lc, uc) match
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LT) => Chunk(ListConstraint.MinSize(l + 1), ListConstraint.MaxSize(u - 1))
                    case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LE) => Chunk(ListConstraint.MinSize(l + 1), ListConstraint.MaxSize(u))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LT) => Chunk(ListConstraint.MinSize(l), ListConstraint.MaxSize(u - 1))
                    case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LE) => Chunk(ListConstraint.MinSize(l), ListConstraint.MaxSize(u))
            ,
            c =>
                val minSize = c.collectFirst { case ListConstraint.MinSize(s) => s }.getOrElse(0)
                val maxSize = c.collectFirst { case ListConstraint.MaxSize(s) => s }.getOrElse(0)
                (minSize, SmallerSizeConstraint.LE, SmallerSizeConstraint.LE, maxSize)
        )
        <> (
            number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("size", ())
        ).transform(
            (l, lc) =>
                lc match
                    case SmallerSizeConstraint.LT => Chunk(ListConstraint.MinSize(l + 1))
                    case SmallerSizeConstraint.LE => Chunk(ListConstraint.MinSize(l))
            ,
            c =>
                val minSize = c.collectFirst { case ListConstraint.MinSize(s) => s }.getOrElse(0)
                (minSize, SmallerSizeConstraint.LE)
        )

    val listConstraint: SchemaSyntax[Chunk[ListConstraint.Constraint]] =
        listSizeConstraints
            <> uniqueByFieldsConstraint.transform(c => Chunk(c), cs => cs.head.asInstanceOf[ListConstraint.UniqueByFields])
            <> uniqueByFieldConstraint.transform(c => Chunk(c), cs => cs.head.asInstanceOf[ListConstraint.UniqueByFields])
            <> uniqueConstraint.transform(c => Chunk(c), cs => cs.head)

    val listConstraintBlock: SchemaSyntax[Chunk[ListConstraint.Constraint]] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~>
            listConstraint.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces)
            <~ whitespaces ~ Syntax.char(']')
    ).transform(
        chunks => chunks.flatten,
        chunk => Chunk(chunk)
    )

    val listOfValues: SchemaSyntax[Schema.ListOfValues] =
        ((item <~ Syntax.char('*')) ~ listConstraintBlock).transform(
            (itemSchema, constraints) => Schema.ListOfValues(itemSchema, constraints.toSeq*),
            items => (items.schema, Chunk.fromIterable(items.constraints))
        )
            <> (item <~ Syntax.char('*')).transform(
                itemSchema => Schema.ListOfValues(itemSchema),
                items => items.schema
            )
            <> ((item <~ Syntax.char('+')) ~ listConstraintBlock).transform(
                (itemSchema, constraints) => Schema.ListOfValues(itemSchema, (ListConstraint.MinSize(1) +: constraints.toSeq)*),
                items => (items.schema, Chunk.fromIterable(items.constraints.filterNot(_ == ListConstraint.MinSize(1))))
            )
            <> (item <~ Syntax.char('+')).transform(
                itemSchema => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(1)),
                items => items.schema
            )

    val tupleValues: SchemaSyntax[Schema.TupleValue] = (Syntax.char('(') ~> (emptyLines ~ whitespaces ~ item).repeatWithSep(whitespaces ~ (Syntax.char(',') <> emptyLines)) <~ whitespaces ~ Syntax.char(')'))
        .filter(values => values.size > 1, s"tuple needs to have at least two items")
        .transform(
            values => Schema.TupleValue(values*),
            tuple => Chunk.fromIterable(tuple.options)
        )

    val alternativeValuesOptions: SchemaSyntax[Schema] = listOfValues.widen[Schema] <> item

    val alternativeValues: SchemaSyntax[Schema.AlternativeValues | Schema.EnumValues] = (alternativeValuesOptions
        .repeatWithSep(whitespaces ~ emptyLines ~ whitespaces ~ Syntax.char('|') ~ whitespaces))
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

    lazy val items: SchemaSyntax[Schema] =
        alternativeValues.widen[Schema]
            <> listOfValues.widen[Schema]
            <> tupleValues.widen[Schema]
            <> item

    val root: SchemaSyntax[Schema] = valueDefinition

    val definition: SchemaSyntax[(String, Schema)] = importStatement <> namedValue

    val schema: SchemaSyntax[(Chunk[(String, Schema)], Option[Schema])] = (
        (emptyLines
            ~ definition.repeatWithSep0(emptyLines)
            ~ emptyLines
            ~ root.optional
            ~ emptyLines ~ whitespaces) <~ Syntax.end
    )
    // .transform(
    //     (c, r) => c ++ r.map(rr => (ROOT_SCHEMA_KEY, rr)).toList,
    //     xs => (xs.filter(x => !isRootSchema(x)), xs.find(isRootSchema).map(_._2))
    // )

/*
    def parse(definition: String): Either[String, Schema] = schema
        .parseString(definition)
        .left
        .map(_.pretty)
        .flatMap(resolveReferencedSchemas)

    def resolveReferencedSchemas(definitions: Chunk[(String, Schema)]): Either[String, Schema] =
        val referencedSchemas                                = definitions.map((k, s) => (k, s, s.dependencies))
        val (dependencyFreeSchemas_, schemaWithDependencies) = referencedSchemas.partition((k, s, d) => d.isEmpty)
        val dependencyFreeSchemas                            = dependencyFreeSchemas_.map((k, s, d) => (k, s))

        dependencyFreeSchemas.isEmpty match
            case true  => Left(s"unresolved references: ${ schemaWithDependencies.flatMap((k, s, d) => s"'${ d }'").distinct.sorted.mkString(", ") }")
            case false =>
                dependencyFreeSchemas.find((k, s) => k == ROOT_SCHEMA_KEY) match
                    case Some((k, s)) => Right(s)
                    case None         =>
                        Schema
                            .sequenceEithers(
                                schemaWithDependencies.map((k, s, d) => s.replaceReferencedValues(dependencyFreeSchemas*).map(ss => (k, ss)))
                            )
                            .flatMap(x => resolveReferencedSchemas(Chunk.fromIterable(x)))
 */
