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

    // ........................................................................

    object TextConstraint:
        sealed abstract class Constraint
        case class Regex(pattern: String)            extends Constraint
        case class Format(format: String)            extends Constraint
        case class Size(bound: BoundConstraint[Int]) extends Constraint

    object ListConstraint:
        sealed abstract class Constraint
        case class Size(bound: BoundConstraint[Int])   extends Constraint
        case object Unique                             extends Constraint
        case class UniqueByFields(fields: Seq[String]) extends Constraint

    object NumericConstraint:
        sealed abstract class Constraint
        case object Integer                                  extends Constraint
        case class Value(bound: BoundConstraint[BigDecimal]) extends Constraint

    object BinaryConstraint:
        sealed abstract class Constraint

        enum BinaryToTextEncoder(val code: String):
            case hex     extends BinaryToTextEncoder("hex")
            case base64  extends BinaryToTextEncoder("base64")
            case base32  extends BinaryToTextEncoder("base32")
            case base58  extends BinaryToTextEncoder("base58")
            case ascii85 extends BinaryToTextEncoder("ascii85")

        case class Encoding(encoder: BinaryToTextEncoder) extends Constraint

        enum BinaryUnit(val symbol: String, val bytes: Integer):
            case bytes extends BinaryUnit("bytes", 1)
            case KB    extends BinaryUnit("KB", 1024)
            case MB    extends BinaryUnit("MB", 1024 * 1024)
            case GB    extends BinaryUnit("GB", 1024 * 1024 * 1024)

        case class Size(bound: BoundConstraint[Int], unit: BinaryUnit) extends Constraint

    // ------------------------------------------------------------------------

    sealed abstract class Schema:
        def doc: Option[String]                                                         = None
        def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] = Right(this)
        def dependencies: Seq[String]                                                   = Seq.empty
        def withDoc(documentation: String): Schema                                      = Documented(Some(documentation), this)
        def withDoc(documentation: Option[String]): Schema                              = documentation match
            case Some(d) => Documented(Some(d), this)
            case None    => this

    final case class Documented(override val doc: Option[String], schema: Schema) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(s => Documented(doc, s))
        override def dependencies: Seq[String]                                                   = schema.dependencies

    final case class Fail()         extends Schema
    final case class AnyValue()     extends Schema
    final case class BooleanValue() extends Schema

    final case class TextValue(constraints: Seq[TextConstraint.Constraint] = Seq.empty, default: Option[String] = None) extends Schema
    object TextValue:
        def apply(constraints: TextConstraint.Constraint*): TextValue = TextValue(constraints.toSeq, None)

    final case class GivenTextValue(value: String) extends Schema

    final case class NumericValue(constraints: Seq[NumericConstraint.Constraint] = Seq.empty, default: Option[BigDecimal] = None) extends Schema
    object NumericValue:
        def apply(constraints: NumericConstraint.Constraint*): NumericValue = NumericValue(constraints.toSeq, None)

    final case class BinaryValue(constraints: BinaryConstraint.Constraint*) extends Schema
    final case class EnumValues(values: String*)                            extends Schema

    final case class ListOfValues(schema: Schema, constraints: ListConstraint.Constraint*) extends Schema:
        override def replaceReferencedValues(context: (String, Schema)*): Either[String, Schema] =
            schema.replaceReferencedValues(context*).map(ListOfValues(_, this.constraints*))
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
