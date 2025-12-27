package rengbis

import zio.Chunk
import zio.parser.{ Syntax, SyntaxOps, StringParserError, StringErrSyntaxOps, AnySyntaxOps, ParserOps, Parser, Printer }
import scala.util.matching.Regex

object Schema:

    // ........................................................................

    sealed abstract class ObjectLabel(val label: String)
    case class MandatoryLabel(value: String) extends ObjectLabel(value)
    case class OptionalLabel (value: String) extends ObjectLabel(value)

    // ........................................................................

    object TextConstraint:
        sealed abstract class Constraint
        case class Regex(regex: scala.util.matching.Regex)  extends Constraint
        case class Format(format: String)                   extends Constraint

        sealed abstract class TextSizeConstraint(val size: Int) extends Constraint
        case class MinLength(override val size: Int)    extends TextSizeConstraint(size)
        case class MaxLength(override val size: Int)    extends TextSizeConstraint(size)
        case class Length(override val size: Int)       extends TextSizeConstraint(size)

    object ListConstraint:
        sealed abstract class Constraint
        case class MinSize(value: Int)      extends Constraint
        case class MaxSize(value: Int)      extends Constraint
        case class ExactSize(value: Int)    extends Constraint

    // ------------------------------------------------------------------------

    sealed abstract class Schema:
        def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] = Right(this)
        def dependencies: Seq[String] = Seq.empty

    final case class Fail()                                             extends Schema
    final case class BooleanValue()                                     extends Schema
    final case class TextValue(constraints: TextConstraint.Constraint*) extends Schema
    final case class GivenTextValue(value: String)                      extends Schema
    final case class NumericValue()                                     extends Schema
    final case class EnumValues(values: String*)                        extends Schema

    final case class ListOfValues(schema: Schema, constraints:ListConstraint.Constraint*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(ListOfValues(_, this.constraints*))
        override def dependencies: Seq[String] = schema.dependencies

    final case class TupleValue(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(AlternativeValues(_*))
        override def dependencies: Seq[String] = options.flatMap(_.dependencies)

    final case class AlternativeValues(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(AlternativeValues(_*))
        override def dependencies: Seq[String] = options.flatMap(_.dependencies)

    final case class ObjectValue(obj: Map[ObjectLabel, Schema]) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(obj.map((k, s) => s.replaceReferencedValues(context*).map((k, _))).toSeq).map(cs => ObjectValue(cs.toMap))
        override def dependencies: Seq[String] = obj.flatMap((k, s) => s.dependencies).toSeq

    final case class NamedValueReference(reference: String) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            context.find((k, _) => k == reference) match
                case Some((k ,s))   =>  Right(s)
                case None           =>  Right(this)
        override def dependencies: Seq[String] = Seq(reference)

    def parse(schemaDefinition: String): Either[String, Schema] =
        SchemaSyntax.parse(schemaDefinition)

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

        val Tab =   '\u0009'
        val Space = '\u0020'
        val LF =    '\u000A'
        val CR =    '\u000D'

        val whitespaces: SchemaSyntax[Unit] = Syntax.charIn(' ', '\t', '\r').*.unit(Chunk())
        // val comment:     SchemaSyntax[Unit] = ((Syntax.string("--", "") <> Syntax.string("//", "")) ~ Syntax.any.repeat0).unit(("", Chunk()))
        // val comment:     SchemaSyntax[Unit] = (Syntax.string("#", "") ~ Syntax.any.repeat0).unit(("", Chunk()))

        val number: SchemaSyntax[Int] = Syntax.digit.repeat.transform(
            chars => chars.mkString.toInt,
            num => Chunk.fromArray(num.toString.toCharArray)
        )
        // val emptyLines:  SchemaSyntax[Unit] = (whitespaces ~ comment.optional ~ Syntax.char('\n')).repeat0.unit(Chunk())
        val emptyLines:  SchemaSyntax[Unit] = (whitespaces ~ Syntax.char('\n')).repeat0.unit(Chunk())

        val quote: SchemaSyntax[Unit]          = Syntax.char('\"')
        val escapedChar: SchemaSyntax[Char]    = Syntax.charNotIn('\"') // TODO: real escaping support
        val quotedString: SchemaSyntax[String] = (quote ~> escapedChar.*.string <~ quote)

        sealed trait SizeConstraint
        enum SmallerSizeConstraint extends SizeConstraint:
            case LT, LE
        enum ExtendedSizeConstraint extends SizeConstraint:
            case EQ, GT, GE

        val smallerSizeConstrint: SchemaSyntax[SmallerSizeConstraint]
            =   Syntax.string("<=", SmallerSizeConstraint.LE)
            <>  Syntax.string("<",  SmallerSizeConstraint.LT)

        val sizeConstraint: SchemaSyntax[SizeConstraint]
            =   Syntax.string("==", ExtendedSizeConstraint.EQ)
            <>  Syntax.string(">=", ExtendedSizeConstraint.GE)
            <>  Syntax.string(">",  ExtendedSizeConstraint.GT)
            <>  smallerSizeConstrint.widen[SizeConstraint]

        val sizeTextConstraints: SchemaSyntax[Chunk[TextConstraint.TextSizeConstraint]]
            =   (Syntax.string("length", ()) ~ whitespaces ~ sizeConstraint ~ whitespaces ~ number).transform(
                    (c, n) => Chunk(
                        c match
                            case ExtendedSizeConstraint.EQ  => TextConstraint.Length(n)
                            case ExtendedSizeConstraint.GT  => TextConstraint.MinLength(n + 1)
                            case ExtendedSizeConstraint.GE  => TextConstraint.MinLength(n)
                            case SmallerSizeConstraint.LT   => TextConstraint.MaxLength(n - 1)
                            case SmallerSizeConstraint.LE   => TextConstraint.MaxLength(n)
                    ),
                    c => c.head match
                        case TextConstraint.Length(l)       =>    (ExtendedSizeConstraint.EQ, l)
                        case TextConstraint.MinLength(l)    =>    (ExtendedSizeConstraint.GE, l)
                        case TextConstraint.MaxLength(l)    =>    (SmallerSizeConstraint.LE, l)
                )
            <>  (number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("length", ()) ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ number).transform(
                    (l, lc, uc, u) => (lc, uc) match
                        case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LT) => Chunk(TextConstraint.MinLength(l + 1), TextConstraint.MaxLength(u - 1))
                        case (SmallerSizeConstraint.LT, SmallerSizeConstraint.LE) => Chunk(TextConstraint.MinLength(l + 1), TextConstraint.MaxLength(u))
                        case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LT) => Chunk(TextConstraint.MinLength(l),     TextConstraint.MaxLength(u - 1))
                        case (SmallerSizeConstraint.LE, SmallerSizeConstraint.LE) => Chunk(TextConstraint.MinLength(l),     TextConstraint.MaxLength(u))
                    ,
                    c => (c.head.size, SmallerSizeConstraint.LE, SmallerSizeConstraint.LE, c.tail.head.size )
                )
            <>  (number ~ whitespaces ~ smallerSizeConstrint ~ whitespaces ~ Syntax.string("length", ())).transform(
                    (l, lc) => lc match
                        case SmallerSizeConstraint.LT   => Chunk(TextConstraint.MinLength(l + 1))
                        case SmallerSizeConstraint.LE   => Chunk(TextConstraint.MinLength(l))
                    ,
                    c => (c.head.size, SmallerSizeConstraint.LE)
                )
        val regexTextConstraint: SchemaSyntax[TextConstraint.Constraint]
            = (Syntax.string("regex", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString).transform(
                text => TextConstraint.Regex(text.r),
                c => c.regex.regex
            ).widen[TextConstraint.Constraint]

        val formatTextConstraint: SchemaSyntax[TextConstraint.Constraint]
            = (Syntax.string("pattern", ()) ~ whitespaces ~ Syntax.char('=') ~ whitespaces ~> quotedString).transform(
                text => TextConstraint.Format(text),
                c => c.format
            ).widen[TextConstraint.Constraint]

        val textConstraints: SchemaSyntax[Chunk[TextConstraint.Constraint]]
            =   sizeTextConstraints.widen[Chunk[TextConstraint.Constraint]]
            <>  regexTextConstraint.+
            <>  formatTextConstraint.+

        val textValue:      SchemaSyntax[Schema.TextValue]      = (Syntax.string("text", ())
            ~ (whitespaces ~ Syntax.char('{') ~ whitespaces ~> textConstraints.repeatWithSep(whitespaces ~ Syntax.char(',') ~ whitespaces) <~ whitespaces ~ Syntax.char('}')).optional
        ).transform(
            constraints => TextValue(constraints.map(_.flatMap(identity)).getOrElse(Chunk()).toList*),
            textValue   => if textValue.constraints.isEmpty then None else Some(Chunk(Chunk.fromIterable(textValue.constraints)))
        )

        val booleanValue:   SchemaSyntax[Schema.BooleanValue]   = Syntax.string("boolean", Schema.BooleanValue())
        val givenTextValue: SchemaSyntax[Schema.GivenTextValue] = quotedString.transform(s => Schema.GivenTextValue(s), v => v.value)
        val numericValue:   SchemaSyntax[Schema.NumericValue]   = Syntax.string("number",  Schema.NumericValue())

        val label: SchemaSyntax[String] = (Syntax.letter ~ (Syntax.alphaNumeric <> Syntax.charIn("_-")).repeat0).string

        val madatoryLabel: SchemaSyntax[ObjectLabel] = label.transform(
            s => MandatoryLabel(s),
            l => l.label
        )
        val optionalLabel: SchemaSyntax[ObjectLabel] = (label <~ Syntax.char('?')).transform(
            s => OptionalLabel(s),
            l => l.label
        )
        val objectLabel: SchemaSyntax[ObjectLabel] = optionalLabel <> madatoryLabel

        val valueDefinition: SchemaSyntax[Schema] = Syntax.char('=') ~ whitespaces ~ items

        val namedValue: SchemaSyntax[(String, Schema)] = label ~ whitespaces ~ valueDefinition

        val namedValueReference: SchemaSyntax[Schema.NamedValueReference] = ((Syntax.letter ~ Syntax.alphaNumeric.repeat0).string).transform(
            label => NamedValueReference(label),
            schema => schema.reference
        )

        val objectValue: SchemaSyntax[Schema.ObjectValue] = (Syntax.char('{') ~>
                (emptyLines ~ whitespaces ~
                    objectLabel ~ Syntax.char(':') ~ whitespaces ~ items
                ).repeatWithSep( whitespaces ~ (Syntax.char(',') <> emptyLines)) ~ whitespaces ~ emptyLines
            <~ whitespaces ~ Syntax.char('}'))
        .transform(
            keys    => Schema.ObjectValue(keys.toMap),
            obj     => Chunk.fromIterable(obj.obj)
        )

        val item: SchemaSyntax[Schema]  =   booleanValue                                                .widen[Schema]
                                        <>  numericValue                                                .widen[Schema]
                                        <>  textValue                                                   .widen[Schema]
                                        <>  givenTextValue                                              .widen[Schema]
                                        <>  namedValueReference                                         .widen[Schema]
                                        <>  (Syntax.char('(') ~ alternativeValues ~ Syntax.char(')'))   .widen[Schema]
                                        <>  objectValue                                                 .widen[Schema]

        val listOfValues: SchemaSyntax[Schema.ListOfValues]
            =   (item <~ Syntax.char('*')).transform(
                    itemSchema => Schema.ListOfValues(itemSchema),
                    items => items.schema
                )
            <>  (item <~ Syntax.char('+')).transform(
                    itemSchema => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(1)),
                    items => items.schema
                )
            <>  (item ~ Syntax.char('{') ~ number ~ Syntax.char('}')).transform(
                    (itemSchema, size) => Schema.ListOfValues(itemSchema, ListConstraint.ExactSize(size)),
                    items => (items.schema, items.constraints.map(c => c match { case ListConstraint.ExactSize(size) => size case _ => 0}).sum)
                )
            <>  (item ~ Syntax.char('{') ~ number ~ Syntax.char(',') ~ Syntax.char('}'))
                .transform(
                    (itemSchema, minSize) => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(minSize)),
                    items => (items.schema, items.constraints.map(c => c match { case ListConstraint.MinSize(size) => size case _ => 0}).max)
                )
            <>  (item ~ Syntax.char('{') ~ number ~ Syntax.char(',') ~ number ~ Syntax.char('}'))
                .filter((schema, minSize, maxSize) => minSize < maxSize, s"lower bound should be smaller that upper bound")
                .transform(
                    (itemSchema, minSize, maxSize) => Schema.ListOfValues(itemSchema, ListConstraint.MinSize(minSize), ListConstraint.MaxSize(maxSize)),
                    items =>
                        val minSize = items.constraints.map(c => c match { case ListConstraint.MinSize(size) => size case _ => 0}).max
                        val maxSize = items.constraints.map(c => c match { case ListConstraint.MaxSize(size) => size case _ => 0}).min
                        (items.schema, minSize, maxSize)
                )

        val tupleValues: SchemaSyntax[Schema.TupleValue] = (Syntax.char('(') ~>
                (emptyLines ~ whitespaces ~ item).repeatWithSep( whitespaces ~ (Syntax.char(',') <> emptyLines))
            <~ whitespaces ~ Syntax.char(')'))
        .filter(values => values.size > 1, s"tuple needs to have at least two items")
        .transform(
            values  => Schema.TupleValue(values*),
            tuple   => Chunk.fromIterable(tuple.options)
        )

        val alternativeValuesOptions: SchemaSyntax[Schema]  =   listOfValues.widen[Schema]
                                                            <>  item
        val alternativeValues: SchemaSyntax[Schema.AlternativeValues | Schema.EnumValues]  = (alternativeValuesOptions.repeatWithSep(whitespaces ~ Syntax.char('|') ~ whitespaces))
        .filter(options => options.size > 1, s"alternatives needs to have at least two items")
        .transform(
            options         =>  if (options.forall(s => s match { case GivenTextValue(_) => true; case _ => false}))
                                    then Schema.EnumValues(options.map(s => s match { case GivenTextValue(value) => value; case _ => "" })*)
                                    else Schema.AlternativeValues(options*),
            alternatives    =>  alternatives match
                                    case EnumValues(values*)            => Chunk.fromIterable(values.map(v => Schema.GivenTextValue(v)))
                                    case AlternativeValues(options*)    => Chunk.fromIterable(options)
        )

        lazy val items: SchemaSyntax[Schema]    =   alternativeValues   .widen[Schema]
                                                <>  listOfValues        .widen[Schema]
                                                <>  tupleValues         .widen[Schema]
                                                <>  item

        val rootKey = "root"
        val root: SchemaSyntax[(String, Schema)] = valueDefinition.transform(
            s       => (rootKey, s),
            (k, s)  => s
        )

        val schema: SchemaSyntax[(Chunk[(String, Schema)], (String, Schema))] =
            (   emptyLines
            ~   namedValue.repeatWithSep0(emptyLines)
            ~   emptyLines
            ~   root
            ~   emptyLines ~ whitespaces
            ) <~ Syntax.end


        def parse(definition: String): Either[String, Schema] = schema
            .parseString(definition).left.map(_.pretty)
            .map((namedValues, rootSchema) => (namedValues :+ rootSchema))
            .flatMap(resolveReferencedSchemas)

        def resolveReferencedSchemas(definitions: Chunk[(String, Schema)]): Either[String, Schema] =
            val referencedSchemas = definitions.map((k, s) => (k, s, s.dependencies))
            val (dependencyFreeSchemas_, schemaWithDependencies) =  referencedSchemas.partition((k, s, d) => d.isEmpty)
            val dependencyFreeSchemas = dependencyFreeSchemas_.map((k, s, d) => (k, s))

            dependencyFreeSchemas.isEmpty match
                case true   => Left(s"unresolved references: ${schemaWithDependencies.flatMap((k, s, d) => s"'${d}'").distinct.sorted.mkString(", ")}")
                case false  =>
                    dependencyFreeSchemas.find((k, s) => k == rootKey) match
                        case Some((k, s))   => Right(s)
                        case None           =>
                            sequenceEithers(
                                schemaWithDependencies.map((k, s, d) => s.replaceReferencedValues(dependencyFreeSchemas*).map(ss => (k, ss)))
                            ).flatMap(x => resolveReferencedSchemas(Chunk.fromIterable(x)))
