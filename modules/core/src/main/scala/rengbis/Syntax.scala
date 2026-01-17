package rengbis

import zio.Chunk
import zio.parser.{ AnySyntaxOps, Parser, ParserOps, Printer, StringErrSyntaxOps, StringParserError, Syntax, SyntaxOps }
import Schema.{ AlternativeValues, BinaryValue, Documented, EnumValues, ImportStatement, MandatoryLabel, NamedValueReference, NumericValue, ObjectLabel, OptionalLabel, Schema, ScopedReference, TextValue }
import Schema.{ BinaryConstraint, BoundConstraint, BoundOp, GivenTextValue, ListConstraint, NumericConstraint, TextConstraint }

object SchemaSyntax:
    import Extensions.*
    import BoundConstraintHelpers.{ boundConstraintSyntax, groupNumericConstraints }

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
        whitespaces0 ~ Syntax.string("##", ()) ~ whitespaces0 ~> Syntax.charNotIn('\n').repeat0.string <~ Syntax.char('\n')
    )
    val precedingDocComment: SchemaSyntax[Option[String]] = docCommentLine.repeat0.transform(
        lines => if lines.isEmpty then None else Some(lines.map(_.trim).mkString("\n")),
        doc => doc.map(s => Chunk.fromIterable(s.split("\n").map(_ + "\n").toSeq)).getOrElse(Chunk.empty)
    )

    // Trailing doc comment: ## at end of line (single line only)
    val trailingDocComment: SchemaSyntax[Option[String]] = (
        whitespaces ~ Syntax.string("##", ()) ~ whitespaces0 ~> Syntax.charNotIn('\n').repeat0.string
    ).optional.transform(
        opt => opt.map(_.trim).filter(_.nonEmpty),
        doc => doc
    )

    val number: SchemaSyntax[Int] = Syntax.digit.repeat.transform(
        chars => chars.mkString.toInt,
        num => Chunk.fromArray(num.toString.toCharArray)
    )

    val emptyLines: SchemaSyntax[Unit] = (whitespaces ~ comment.optional ~ Syntax.char('\n')).repeat0.unit(Chunk())

    val quote: SchemaSyntax[Unit]          = Syntax.char('\"')
    val escapedChar: SchemaSyntax[Char]    = Syntax.charNotIn('\"') // TODO: real escaping support
    val quotedString: SchemaSyntax[String] = (quote ~> escapedChar.*.string <~ quote)

    val sizeTextConstraints: SchemaSyntax[Chunk[TextConstraint.Size]] = boundConstraintSyntax(Syntax.string("length", ()), number)
        .transform(
            bounds => bounds.map((_, b) => TextConstraint.Size(b)),
            sizes => sizes.map(s => ((), s.bound))
        )

    val regexTextConstraint: SchemaSyntax[TextConstraint.Regex] = (
        Syntax.string("regex", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    ).transform(
        text => TextConstraint.Regex(text),
        c => c.pattern
    )

    val formatTextConstraint: SchemaSyntax[TextConstraint.Format] = (
        Syntax.string("pattern", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString
    ).transform(
        text => TextConstraint.Format(text),
        c => c.format
    )

    val textConstraints: SchemaSyntax[Chunk[TextConstraint.Constraint]] =
        sizeTextConstraints.asChunkOf[TextConstraint.Constraint] { case s: TextConstraint.Size => s }
            <> regexTextConstraint.as[TextConstraint.Constraint] { case r: TextConstraint.Regex => r }.+
            <> formatTextConstraint.as[TextConstraint.Constraint] { case f: TextConstraint.Format => f }.+

    val textConstraintBlock: SchemaSyntax[Chunk[TextConstraint.Constraint]] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> textConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')
    ).transform(_.flatten, Chunk(_))

    val textDefaultValue: SchemaSyntax[String] = whitespaces ~ Syntax.string("?=", ()) ~ whitespaces ~> quotedString

    val textValue: SchemaSyntax[Schema.TextValue] = (
        Syntax.string("text", ()) ~ textConstraintBlock.optional ~ textDefaultValue.optional
    ).transform(
        { case (constraints, default) => TextValue(constraints.getOrElse(Chunk()).toSeq, default) },
        textValue =>
            (
                if textValue.constraints.isEmpty then None else Some(Chunk.fromIterable(textValue.constraints)),
                textValue.default
            )
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

    val integerConstraint: SchemaSyntax[NumericConstraint.Integer.type] = Syntax.string("integer", NumericConstraint.Integer)

    val valueNumericConstraints: SchemaSyntax[Chunk[NumericConstraint.Value]] = boundConstraintSyntax(Syntax.string("value", ()), decimalNumber)
        .transform(
            bounds => bounds.map((_, b) => NumericConstraint.Value(b)),
            values => values.map(v => ((), v.bound))
        )

    val numericConstraints: SchemaSyntax[Chunk[NumericConstraint.Constraint]] =
        valueNumericConstraints.asChunkOf[NumericConstraint.Constraint] { case v: NumericConstraint.Value => v }
            <> integerConstraint.as[NumericConstraint.Constraint] { case NumericConstraint.Integer => NumericConstraint.Integer }.+

    val numericConstraintBlock: SchemaSyntax[Chunk[NumericConstraint.Constraint]] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~> numericConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')
    ).transform(_.flatten, groupNumericConstraints(_))

    val numericDefaultValue: SchemaSyntax[BigDecimal] = whitespaces ~ Syntax.string("?=", ()) ~ whitespaces ~> decimalNumber

    val numericValue: SchemaSyntax[Schema.NumericValue] = (
        Syntax.string("number", ()) ~ numericConstraintBlock.optional ~ numericDefaultValue.optional
    ).transform(
        { case (constraints, default) => NumericValue(constraints.getOrElse(Chunk()).toSeq, default) },
        numericValue =>
            (
                if numericValue.constraints.isEmpty then None else Some(Chunk.fromIterable(numericValue.constraints)),
                numericValue.default
            )
    )

    val binaryToTextEncoder: SchemaSyntax[BinaryConstraint.BinaryToTextEncoder] =
        Syntax.string("'hex'", BinaryConstraint.BinaryToTextEncoder.hex)
            <> Syntax.string("'base64'", BinaryConstraint.BinaryToTextEncoder.base64)
            <> Syntax.string("'base32'", BinaryConstraint.BinaryToTextEncoder.base32)
            <> Syntax.string("'base58'", BinaryConstraint.BinaryToTextEncoder.base58)
            <> Syntax.string("'ascii85'", BinaryConstraint.BinaryToTextEncoder.ascii85)

    val encodingConstraint: SchemaSyntax[BinaryConstraint.Encoding] = (
        Syntax.string("encoding", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> binaryToTextEncoder
    ).transform(
        encoder => BinaryConstraint.Encoding(encoder),
        c => c.encoder
    )

    val sizeBinaryUnit: SchemaSyntax[BinaryConstraint.BinaryUnit] =
        Syntax.string("bytes", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("bits", BinaryConstraint.BinaryUnit.bytes)
            <> Syntax.string("KB", BinaryConstraint.BinaryUnit.KB)
            <> Syntax.string("MB", BinaryConstraint.BinaryUnit.MB)
            <> Syntax.string("GB", BinaryConstraint.BinaryUnit.GB)

    val sizeBinaryConstraints: SchemaSyntax[Chunk[BinaryConstraint.Size]] = boundConstraintSyntax(sizeBinaryUnit, number)
        .transform(
            bounds =>
                bounds.map((unit, b) =>
                    val normalizedValue = b.value * unit.bytes
                    BinaryConstraint.Size(BoundConstraint(b.op, normalizedValue), BinaryConstraint.BinaryUnit.bytes)
                ),
            sizes => sizes.map(s => (BinaryConstraint.BinaryUnit.bytes, s.bound))
        )

    val binaryConstraints: SchemaSyntax[Chunk[BinaryConstraint.Constraint]] =
        sizeBinaryConstraints.asChunkOf[BinaryConstraint.Constraint] { case s: BinaryConstraint.Size => s }
            <> encodingConstraint.as[BinaryConstraint.Constraint] { case e: BinaryConstraint.Encoding => e }.+

    val binaryValue: SchemaSyntax[Schema.BinaryValue] = (
        Syntax.string("binary", ()) ~ (whitespaces ~ Syntax.char('[') ~ whitespaces ~> binaryConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char(']')).optional
    ).transform(
        constraints => BinaryValue(constraints.map(_.flatMap(identity)).getOrElse(Chunk()).toList*),
        binaryValue => if binaryValue.constraints.isEmpty then None else Some(Chunk(Chunk.fromIterable(binaryValue.constraints)))
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

    // Named value with optional doc comments (preceding ## lines and/or trailing ##)
    val namedValue: SchemaSyntax[(String, Schema)] = (
        precedingDocComment ~ whitespaces0 ~ label ~ whitespaces ~ valueDefinition ~ trailingDocComment
    ).transform(
        { case (precedingDoc, name, schema, trailingDoc) =>
            val doc = Documentation.combineDoc(precedingDoc, trailingDoc)
            (name, schema.withDoc(doc))
        },
        { case (name, schema) =>
            schema match
                case Documented(doc, inner) => (doc, name, inner, None)
                case _                      => (None, name, schema, None)
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

    // Object field with optional doc comments (preceding ## lines and/or trailing ##)
    val objectField: SchemaSyntax[(ObjectLabel, Schema)] = (
        precedingDocComment ~ whitespaces0 ~ objectLabel ~ Syntax.char(':') ~ whitespaces ~ items ~ trailingDocComment
    ).transform(
        { case (precedingDoc, label, schema, trailingDoc) =>
            val doc = Documentation.combineDoc(precedingDoc, trailingDoc)
            (label, schema.withDoc(doc))
        },
        { case (label, schema) =>
            schema match
                case Documented(doc, inner) => (doc, label, inner, None)
                case _                      => (None, label, schema, None)
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
            <> textValue.asSchema { case t: Schema.TextValue => t }
            <> givenTextValue.asSchema { case g: Schema.GivenTextValue => g }
            <> mapValue.asSchema { case m: Schema.MapValue => m }
            <> objectValue.asSchema { case o: Schema.ObjectValue => o; case d @ Documented(_, _: Schema.ObjectValue) => d }
            <> groupedAlternatives.asSchema { case e: Schema.EnumValues => e; case a: Schema.AlternativeValues => a }
            <> tupleValues.asSchema { case t: Schema.TupleValue => t }
            <> scopedReference.asSchema { case s: Schema.ScopedReference => s }
            <> namedValueReference.asSchema { case n: Schema.NamedValueReference => n }

    val uniqueConstraint: SchemaSyntax[ListConstraint.Unique.type]            = Syntax.string("unique", ListConstraint.Unique)
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

    val listSizeConstraints: SchemaSyntax[Chunk[ListConstraint.Size]] = boundConstraintSyntax(Syntax.string("size", ()), number)
        .transform(
            bounds => bounds.map((_, b) => ListConstraint.Size(b)),
            sizes => sizes.map(s => ((), s.bound))
        )

    val listConstraint: SchemaSyntax[Chunk[ListConstraint.Constraint]] =
        listSizeConstraints.asChunkOf[ListConstraint.Constraint] { case s: ListConstraint.Size => s }
            <> uniqueByFieldsConstraint.as[ListConstraint.Constraint] { case u: ListConstraint.UniqueByFields if u.fields.size > 1 => u }.+
            <> uniqueByFieldConstraint.as[ListConstraint.Constraint] { case u: ListConstraint.UniqueByFields if u.fields.size == 1 => u }.+
            <> uniqueConstraint.as[ListConstraint.Constraint] { case ListConstraint.Unique => ListConstraint.Unique }.+

    val listConstraintBlock: SchemaSyntax[Chunk[ListConstraint.Constraint]] = (
        whitespaces ~ Syntax.char('[') ~ whitespaces ~>
            listConstraint.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces)
            <~ whitespaces ~ Syntax.char(']')
    ).transform(
        chunks => chunks.flatten,
        chunk => Chunk(chunk)
    )

    val nonEmptyMarker: SchemaSyntax[Boolean] = Syntax.string("+", true) <> Syntax.string("*", false)

    val listSuffix: SchemaSyntax[Chunk[ListConstraint.Constraint]] =
        val atLeastOne = ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, 1))

        (nonEmptyMarker ~ listConstraintBlock.optional).transform(
            (isPlus, extra) =>
                val base = if isPlus then Chunk(atLeastOne) else Chunk.empty
                base ++ extra.getOrElse(Chunk.empty)
            ,
            constraints =>
                val hasMinOne = constraints.contains(atLeastOne)
                val others    = constraints.filterNot(_ == atLeastOne)
                (hasMinOne, if others.isEmpty then None else Some(others))
        )

    val listOfValues: SchemaSyntax[Schema.ListOfValues] =
        (item ~ listSuffix).transform(
            (schema, constraints) => Schema.ListOfValues(schema, constraints.toSeq*),
            list => (list.schema, Chunk.fromIterable(list.constraints))
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

        def groupNumericConstraints(constraints: Seq[NumericConstraint.Constraint]): Chunk[Chunk[NumericConstraint.Constraint]] =
            val integers = constraints.collect { case i: NumericConstraint.Integer.type => i }
            val values   = constraints.collect { case v: NumericConstraint.Value => v }
            val result   = Chunk.fromIterable(integers.map(i => Chunk(i: NumericConstraint.Constraint))) ++
                (if values.nonEmpty then Chunk(Chunk.fromIterable(values)) else Chunk.empty)
            result

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
