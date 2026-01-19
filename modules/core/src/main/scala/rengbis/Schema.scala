package rengbis

import java.nio.file.{ Path, Paths }

object Schema:

    // ........................................................................

    sealed abstract class ObjectLabel(val label: String)
    case class MandatoryLabel(value: String) extends ObjectLabel(value)
    case class OptionalLabel(value: String)  extends ObjectLabel(value)

    // ........................................................................

    enum BoundOp:
        case Exact, MinInclusive, MinExclusive, MaxInclusive, MaxExclusive

    case class BoundConstraint[V](op: BoundOp, value: V):
        def isExact: Boolean        = op == BoundOp.Exact
        def isMinInclusive: Boolean = op == BoundOp.MinInclusive
        def isMinExclusive: Boolean = op == BoundOp.MinExclusive
        def isMaxInclusive: Boolean = op == BoundOp.MaxInclusive
        def isMaxExclusive: Boolean = op == BoundOp.MaxExclusive

    /** Generic bounded range with optional min and/or max bounds */
    case class BoundedRange[V](
        min: Option[BoundConstraint[V]] = None,
        max: Option[BoundConstraint[V]] = None
    ):
        def isEmpty: Boolean = min.isEmpty && max.isEmpty

        /** Merge two BoundedRanges, combining min from one and max from the other */
        def merge(other: BoundedRange[V]): BoundedRange[V] = BoundedRange(
            min = this.min.orElse(other.min),
            max = this.max.orElse(other.max)
        )

    object BoundedRange:
        def exact[V](v: V): BoundedRange[V]                                             = BoundedRange(Some(BoundConstraint(BoundOp.Exact, v)), None)
        def minInclusive[V](v: V): BoundedRange[V]                                      = BoundedRange(Some(BoundConstraint(BoundOp.MinInclusive, v)), None)
        def minExclusive[V](v: V): BoundedRange[V]                                      = BoundedRange(Some(BoundConstraint(BoundOp.MinExclusive, v)), None)
        def maxInclusive[V](v: V): BoundedRange[V]                                      = BoundedRange(None, Some(BoundConstraint(BoundOp.MaxInclusive, v)))
        def maxExclusive[V](v: V): BoundedRange[V]                                      = BoundedRange(None, Some(BoundConstraint(BoundOp.MaxExclusive, v)))
        def range[V](min: BoundConstraint[V], max: BoundConstraint[V]): BoundedRange[V] = BoundedRange(Some(min), Some(max))
        def fromBounds[V](bounds: Seq[BoundConstraint[V]]): BoundedRange[V]             =
            val minBound = bounds.find(b => b.isMinInclusive || b.isMinExclusive || b.isExact)
            val maxBound = bounds.find(b => b.isMaxInclusive || b.isMaxExclusive)
            BoundedRange(minBound, maxBound)

    // ........................................................................

    object TextConstraint:
        type SizeRange = BoundedRange[Int]
        val SizeRange = BoundedRange

        /** Structured text constraints - each optional, at most one of each type */
        case class Constraints(
            size: Option[SizeRange] = None,
            regex: Option[String] = None,
            format: Option[String] = None
        ):
            def isEmpty: Boolean = size.isEmpty && regex.isEmpty && format.isEmpty

        object Constraints:
            val empty: Constraints = Constraints()

    object ListConstraint:
        type SizeRange = BoundedRange[Int]
        val SizeRange = BoundedRange

        /** Uniqueness constraint - either simple unique or unique by specific fields (composite key) */
        enum Uniqueness:
            case Simple
            case ByFields(fields: Seq[String]) // Composite key - uniqueness on the combination of fields

        /** Structured list constraints Note: unique is a Seq to support multiple independent uniqueness constraints. Example: `unique = id, unique = code` means id must be unique AND code must be unique (independently). Example: `unique = (id, code)` means the (id, code) combination must be unique (composite key).
          */
        case class Constraints(
            size: Option[SizeRange] = None,
            unique: Seq[Uniqueness] = Seq.empty
        ):
            def isEmpty: Boolean = size.isEmpty && unique.isEmpty

        object Constraints:
            val empty: Constraints = Constraints()

    object NumericConstraint:
        type ValueRange = BoundedRange[BigDecimal]
        val ValueRange = BoundedRange

        /** Structured numeric constraints */
        case class Constraints(
            value: Option[ValueRange] = None,
            integer: Boolean = false
        ):
            def isEmpty: Boolean = value.isEmpty && !integer

        object Constraints:
            val empty: Constraints = Constraints()

    object BinaryConstraint:
        enum BinaryToTextEncoder(val code: String):
            case hex     extends BinaryToTextEncoder("hex")
            case base64  extends BinaryToTextEncoder("base64")
            case base32  extends BinaryToTextEncoder("base32")
            case base58  extends BinaryToTextEncoder("base58")
            case ascii85 extends BinaryToTextEncoder("ascii85")

        enum BinaryUnit(val symbol: String, val bytes: Integer):
            case bytes extends BinaryUnit("bytes", 1)
            case KB    extends BinaryUnit("KB", 1024)
            case MB    extends BinaryUnit("MB", 1024 * 1024)
            case GB    extends BinaryUnit("GB", 1024 * 1024 * 1024)

        type SizeRange = BoundedRange[Int]
        val SizeRange = BoundedRange

        /** Structured binary constraints */
        case class Constraints(
            size: Option[SizeRange] = None,
            encoding: Option[BinaryToTextEncoder] = None
        ):
            def isEmpty: Boolean = size.isEmpty && encoding.isEmpty

        object Constraints:
            val empty: Constraints = Constraints()

    object TimeConstraint:
        sealed abstract class Constraint

        sealed abstract class FormatConstraint extends Constraint:
            def formatter: java.time.format.DateTimeFormatter
            def name: String

        enum NamedFormat(val name: String, val formatter: java.time.format.DateTimeFormatter) extends FormatConstraint:
            case ISO8601          extends NamedFormat("iso8601", java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            case ISO8601_DateTime extends NamedFormat("iso8601.datetime", java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            case ISO8601_Date     extends NamedFormat("iso8601.date", java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
            case ISO8601_Time     extends NamedFormat("iso8601.time", java.time.format.DateTimeFormatter.ISO_LOCAL_TIME)
            case RFC3339          extends NamedFormat("rcf3339", java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)

        case class CustomPattern(pattern: String) extends FormatConstraint:
            val formatter: java.time.format.DateTimeFormatter = java.time.format.DateTimeFormatter.ofPattern(pattern)
            val name                                          = "custom"

    // ------------------------------------------------------------------------

    sealed abstract class Schema:
        def doc: Option[String]                                                         = None
        def deprecated: Boolean                                                         = false
        def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] = Right(this)
        def dependencies: Seq[String]                                                   = Seq.empty

        def withDoc(documentation: Option[String]): Schema = documentation match
            case Some(d) => Documented(Some(d), this)
            case None    => this
        def asDeprecated: Schema                           = Deprecated(this)

    final case class Documented(override val doc: Option[String], schema: Schema) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(s => Documented(doc, s))
        override def dependencies: Seq[String]                                                   = schema.dependencies

    final case class Deprecated(schema: Schema) extends Schema:
        override val deprecated: Boolean                                                         = true
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(s => Deprecated(s))
        override def dependencies: Seq[String]                                                   = schema.dependencies

    final case class Fail()                                        extends Schema
    final case class AnyValue()                                    extends Schema
    final case class BooleanValue(default: Option[Boolean] = None) extends Schema

    final case class TextValue(constraints: TextConstraint.Constraints = TextConstraint.Constraints.empty, default: Option[String] = None) extends Schema
    object TextValue:
        def apply(): TextValue = TextValue(TextConstraint.Constraints.empty, None)

    final case class GivenTextValue(value: String) extends Schema

    final case class NumericValue(constraints: NumericConstraint.Constraints = NumericConstraint.Constraints.empty, default: Option[BigDecimal] = None) extends Schema
    object NumericValue:
        def apply(): NumericValue = NumericValue(NumericConstraint.Constraints.empty, None)

    final case class BinaryValue(constraints: BinaryConstraint.Constraints = BinaryConstraint.Constraints.empty) extends Schema
    object BinaryValue:
        def apply(): BinaryValue = BinaryValue(BinaryConstraint.Constraints.empty)

    final case class TimeValue(constraints: TimeConstraint.Constraint*) extends Schema

    final case class EnumValues(values: String*) extends Schema

    final case class ListOfValues(schema: Schema, constraints: ListConstraint.Constraints = ListConstraint.Constraints.empty) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(ListOfValues(_, this.constraints))
        override def dependencies: Seq[String]                                                   =
            schema.dependencies

    final case class TupleValue(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(TupleValue(_*))
        override def dependencies: Seq[String]                                                   =
            options.flatMap(_.dependencies)

    final case class AlternativeValues(options: Schema*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(options.map(_.replaceReferencedValues(context*))).map(AlternativeValues(_*))
        override def dependencies: Seq[String]                                                   =
            options.flatMap(_.dependencies)

    final case class ObjectValue(obj: Map[ObjectLabel, Schema]) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            sequenceEithers(obj.map((k, s) => s.replaceReferencedValues(context*).map((k, _))).toSeq).map(cs => ObjectValue(cs.toMap))
        override def dependencies: Seq[String]                                                   =
            obj.flatMap((k, s) => s.dependencies).toSeq

    final case class MapValue(valueSchema: Schema) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            valueSchema.replaceReferencedValues(context*).map(MapValue(_))
        override def dependencies: Seq[String]                                                   =
            valueSchema.dependencies

    final case class NamedValueReference(reference: String) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            context.find((k, _) => k == reference) match
                case Some((k, s)) => Right(s)
                case None         => Right(this)
        override def dependencies: Seq[String]                                                   =
            Seq(reference)

    final case class ImportStatement(namespace: String, path: String) extends Schema:
        override def dependencies: Seq[String] = Seq.empty

    final case class ScopedReference(namespace: String, name: String) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            val scopedName = if name.isEmpty then namespace else s"$namespace.$name"
            context.find((k, _) => k == scopedName) match
                case Some((k, s)) => Right(s)
                case None         => Right(this)
        override def dependencies: Seq[String]                                                   =
            Seq(if name.isEmpty then namespace else s"$namespace.$name")

    def parse(schemaDefinition: String): Either[String, rengbis.ParsedSchema] =
        SchemaSyntax.schema
            .parseString(schemaDefinition)
            .left
            .map(_.pretty)
            .map((scopes, root) => {
                rengbis.ParsedSchema(
                    root = root,
                    definitions = scopes.collect { case (name, schema) if !schema.isInstanceOf[ImportStatement] => (name, schema) }.toMap,
                    imports = scopes.collect { case (name, imp: ImportStatement) => (name, Paths.get(imp.path)) }.toMap
                )
            })

// ====================================================================

class ParsedSchema(root: Option[Schema.Schema], definitions: Map[String, Schema.Schema], val imports: Map[String, Path]) extends ResolvedSchema(root, definitions):
    override def toString() = s"ParsedSchema: root: ${ root }, definitions: ${ definitions }, imports: ${ imports }"

class ResolvedSchema(val root: Option[Schema.Schema], val definitions: Map[String, Schema.Schema]):
    override def toString()                          = s"ResolvedSchema: root: ${ root }, definitions: ${ definitions }"
    def getRootSchema: Either[String, Schema.Schema] = root match
        case None         => Left("root schema not available")
        case Some(schema) => Right(schema)
