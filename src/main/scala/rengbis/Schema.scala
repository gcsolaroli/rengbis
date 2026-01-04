package rengbis

import zio.Chunk
import zio.parser.{ AnySyntaxOps, Parser, ParserOps, Printer, StringErrSyntaxOps, StringParserError, Syntax, SyntaxOps }
import scala.util.matching.Regex

object Schema:

    // ........................................................................

    sealed abstract class ObjectLabel(val label: String)
    case class MandatoryLabel(value: String) extends ObjectLabel(value)
    case class OptionalLabel(value: String)  extends ObjectLabel(value)

    // ........................................................................

    object TextConstraint:
        sealed abstract class Constraint
        case class Regex(pattern: String) extends Constraint
        case class Format(format: String) extends Constraint

        sealed abstract class TextSizeConstraint(val size: Int) extends Constraint
        case class MinLength(override val size: Int)            extends TextSizeConstraint(size)
        case class MaxLength(override val size: Int)            extends TextSizeConstraint(size)
        case class Length(override val size: Int)               extends TextSizeConstraint(size)

    object ListConstraint:
        sealed abstract class Constraint
        case class MinSize(value: Int)   extends Constraint
        case class MaxSize(value: Int)   extends Constraint
        case class ExactSize(value: Int) extends Constraint

    object NumericConstraint:
        sealed abstract class Constraint
        case object Integer extends Constraint

        sealed abstract class NumericValueConstraint(val bound: BigDecimal) extends Constraint
        case class MinValue(override val bound: BigDecimal)                 extends NumericValueConstraint(bound)
        case class MinValueExclusive(override val bound: BigDecimal)        extends NumericValueConstraint(bound)
        case class MaxValue(override val bound: BigDecimal)                 extends NumericValueConstraint(bound)
        case class MaxValueExclusive(override val bound: BigDecimal)        extends NumericValueConstraint(bound)
        case class ExactValue(override val bound: BigDecimal)               extends NumericValueConstraint(bound)

    // ------------------------------------------------------------------------

    sealed abstract class Schema:
        def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] = Right(this)
        def dependencies: Seq[String]                                                   = Seq.empty

    final case class Fail()                                                   extends Schema
    final case class BooleanValue()                                           extends Schema
    final case class TextValue(constraints: TextConstraint.Constraint*)       extends Schema
    final case class GivenTextValue(value: String)                            extends Schema
    final case class NumericValue(constraints: NumericConstraint.Constraint*) extends Schema
    final case class EnumValues(values: String*)                              extends Schema

    final case class ListOfValues(schema: Schema, constraints: ListConstraint.Constraint*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(ListOfValues(_, this.constraints*))
        override def dependencies: Seq[String]                                                   = schema.dependencies

    final case class TupleValue(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(AlternativeValues(_*))
        override def dependencies: Seq[String]                                                   = options.flatMap(_.dependencies)

    final case class AlternativeValues(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(AlternativeValues(_*))
        override def dependencies: Seq[String]                                                   = options.flatMap(_.dependencies)

    final case class ObjectValue(obj: Map[ObjectLabel, Schema]) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(obj.map((k, s) => s.replaceReferencedValues(context*).map((k, _))).toSeq).map(cs => ObjectValue(cs.toMap))
        override def dependencies: Seq[String]                                                   = obj.flatMap((k, s) => s.dependencies).toSeq

    final case class MapValue(valueSchema: Schema) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            valueSchema.replaceReferencedValues(context*).map(MapValue(_))
        override def dependencies: Seq[String]                                                   = valueSchema.dependencies

    final case class NamedValueReference(reference: String) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            context.find((k, _) => k == reference) match
                case Some((k, s)) => Right(s)
                case None         => Right(this)
        override def dependencies: Seq[String]                                                   = Seq(reference)

    final case class ImportStatement(namespace: String, path: String) extends Schema:
        override def dependencies: Seq[String] = Seq.empty

    final case class ScopedReference(namespace: String, name: String) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            val scopedName = if name.isEmpty then namespace else s"$namespace.$name"
            context.find((k, _) => k == scopedName) match
                case Some((k, s)) => Right(s)
                case None         => Right(this)
        override def dependencies: Seq[String]                                                   = Seq(if name.isEmpty then namespace else s"$namespace.$name")

    def parse(schemaDefinition: String): Either[String, Schema] =
        SchemaSyntax.parse(schemaDefinition)

    def parseFile(path: java.nio.file.Path): Either[String, Schema] =
        SchemaLoader.loadAndResolve(path).map(_.rootSchema)

    def sequenceEithers[E, A](list: Seq[Either[E, A]]): Either[E, Seq[A]] =
        list.foldLeft[Either[E, Seq[A]]](Right(Seq.empty)) { (acc, current) =>
            for
                accumulator <- acc
                value       <- current
            yield accumulator :+ value
        }

// ====================================================================

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
            (Syntax.letter ~ Syntax.alphaNumeric.repeat0).string
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

        val item: SchemaSyntax[Schema] = booleanValue.widen[Schema]
            <> numericValue.widen[Schema]
            <> textValue.widen[Schema]
            <> givenTextValue.widen[Schema]
            <> mapValue.widen[Schema]
            <> objectValue.widen[Schema]
            <> (Syntax.char('(') ~ whitespaces ~> alternativeValues <~ whitespaces ~ Syntax.char(')')).widen[Schema]
            <> scopedReference.widen[Schema]
            <> namedValueReference.widen[Schema]

        val listOfValues: SchemaSyntax[Schema.ListOfValues] = (item <~ Syntax.char('*')).transform(
            itemSchema => Schema.ListOfValues(itemSchema),
            items => items.schema
        )
            <> (item <~ Syntax.char('+')).transform(
                itemSchema => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(1)),
                items => items.schema
            )
            <> (item ~ Syntax.char('{') ~ number ~ Syntax.char('}')).transform(
                (itemSchema, size) => Schema.ListOfValues(itemSchema, ListConstraint.ExactSize(size)),
                items =>
                    (
                        items.schema,
                        items.constraints
                            .map(c =>
                                c match {
                                    case ListConstraint.ExactSize(size) => size
                                    case _                              => 0
                                }
                            )
                            .sum
                    )
            )
            <> (item ~ Syntax.char('{') ~ number ~ Syntax.char(',') ~ Syntax.char('}'))
                .transform(
                    (itemSchema, minSize) => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(minSize)),
                    items =>
                        (
                            items.schema,
                            items.constraints
                                .map(c =>
                                    c match {
                                        case ListConstraint.MinSize(size) => size
                                        case _                            => 0
                                    }
                                )
                                .max
                        )
                )
            <> (item ~ Syntax.char('{') ~ number ~ Syntax.char(',') ~ number ~ Syntax.char('}'))
                .filter((schema, minSize, maxSize) => minSize < maxSize, s"lower bound should be smaller that upper bound")
                .transform(
                    (itemSchema, minSize, maxSize) => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(minSize), ListConstraint.MaxSize(maxSize)),
                    items =>
                        val minSize = items.constraints
                            .map(c =>
                                c match {
                                    case ListConstraint.MinSize(size) => size
                                    case _                            => 0
                                }
                            )
                            .max
                        val maxSize = items.constraints
                            .map(c =>
                                c match {
                                    case ListConstraint.MaxSize(size) => size
                                    case _                            => 0
                                }
                            )
                            .min
                        (items.schema, minSize, maxSize)
                )

        val tupleValues: SchemaSyntax[Schema.TupleValue] = (Syntax.char('(') ~>
            (emptyLines ~ whitespaces ~ item).repeatWithSep(whitespaces ~ (Syntax.char(',') <> emptyLines))
            <~ whitespaces ~ Syntax.char(')'))
            .filter(values => values.size > 1, s"tuple needs to have at least two items")
            .transform(
                values => Schema.TupleValue(values*),
                tuple => Chunk.fromIterable(tuple.options)
            )

        val alternativeValuesOptions: SchemaSyntax[Schema]                                = listOfValues.widen[Schema]
            <> item
        val alternativeValues: SchemaSyntax[Schema.AlternativeValues | Schema.EnumValues] = (alternativeValuesOptions
            .repeatWithSep(whitespaces ~ Syntax.char('|') ~ whitespaces))
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

        lazy val items: SchemaSyntax[Schema] = alternativeValues.widen[Schema]
            <> listOfValues.widen[Schema]
            <> tupleValues.widen[Schema]
            <> item

        val rootKey = "root"

        val root: SchemaSyntax[(String, Schema)] = valueDefinition.transform(
            s => (rootKey, s),
            (k, s) => s
        )

        val definition: SchemaSyntax[(String, Schema)] = importStatement <> namedValue

        val schema: SchemaSyntax[(Chunk[(String, Schema)], Option[(String, Schema)])] =
            (emptyLines
                ~ definition.repeatWithSep0(emptyLines)
                ~ emptyLines
                ~ root.optional
                ~ emptyLines ~ whitespaces) <~ Syntax.end

        def parse(definition: String): Either[String, Schema] = schema
            .parseString(definition)
            .left
            .map(_.pretty)
            .flatMap((definitions, rootSchema) =>
                rootSchema match
                    case Some(root) => resolveReferencedSchemas(definitions :+ root)
                    case None       => Left("No root schema defined (use '= ...' to define root schema)")
            )

        def resolveReferencedSchemas(definitions: Chunk[(String, Schema)]): Either[String, Schema] =
            val referencedSchemas                                = definitions.map((k, s) => (k, s, s.dependencies))
            val (dependencyFreeSchemas_, schemaWithDependencies) = referencedSchemas.partition((k, s, d) => d.isEmpty)
            val dependencyFreeSchemas                            = dependencyFreeSchemas_.map((k, s, d) => (k, s))

            dependencyFreeSchemas.isEmpty match
                case true  => Left(s"unresolved references: ${ schemaWithDependencies.flatMap((k, s, d) => s"'${ d }'").distinct.sorted.mkString(", ") }")
                case false =>
                    dependencyFreeSchemas.find((k, s) => k == rootKey) match
                        case Some((k, s)) => Right(s)
                        case None         =>
                            sequenceEithers(
                                schemaWithDependencies.map((k, s, d) => s.replaceReferencedValues(dependencyFreeSchemas*).map(ss => (k, ss)))
                            ).flatMap(x => resolveReferencedSchemas(Chunk.fromIterable(x)))
