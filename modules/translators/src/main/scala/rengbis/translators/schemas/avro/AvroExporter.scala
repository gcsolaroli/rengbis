package rengbis.translators.schemas.avro

import rengbis.Schema.*
import rengbis.translators.common.{ FrictionReport, FrictionType }
import zio.json.*
import zio.json.ast.Json

object AvroExporter:

    case class TranslationContext(
        path: String,
        report: FrictionReport,
        nameCounter: Int = 0
    ):
        def withPath(newPath: String): TranslationContext =
            copy(path = newPath)

        def addFriction(frictionType: FrictionType, message: String): TranslationContext =
            val updatedReport = frictionType match
                case FrictionType.Loss          => report.addLoss(path, message)
                case FrictionType.Approximation => report.addApproximation(path, message)
                case FrictionType.Extension     => report.addExtension(path, message)
            copy(report = updatedReport)

        def nextName(prefix: String): (String, TranslationContext) =
            val name = s"${ prefix }${ nameCounter }"
            (name, copy(nameCounter = nameCounter + 1))

    def toAvro(schema: Schema, recordName: String = "Record"): (Json, FrictionReport) =
        val context              = TranslationContext("", FrictionReport())
        val (json, finalContext) = translateSchema(schema, context, Some(recordName))
        (json, finalContext.report)

    private def translateSchema(schema: Schema, context: TranslationContext, nameHint: Option[String] = None): (Json, TranslationContext) =
        schema match
            case AnyValue() =>
                // Avro doesn't have "any" type - use union of all basic types
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "ReNGBis 'any' type approximated as union of basic Avro types"
                )
                (
                    Json.Arr(
                        Json.Str("null"),
                        Json.Str("boolean"),
                        Json.Str("int"),
                        Json.Str("long"),
                        Json.Str("float"),
                        Json.Str("double"),
                        Json.Str("string"),
                        Json.Str("bytes")
                    ),
                    updatedContext
                )

            case BooleanValue(_) =>
                (Json.Str("boolean"), context)

            case TextValue(constraints, defaultValue) =>
                val baseContext =
                    if !constraints.isEmpty then
                        context.addFriction(
                            FrictionType.Loss,
                            s"Text constraints cannot be represented in Avro"
                        )
                    else context
                (Json.Str("string"), baseContext)

            case NumericValue(constraints, defaultValue) =>
                val isInteger = constraints.integer

                val hasLargeNumbers = constraints.value.exists { valueRange =>
                    valueRange.min.exists(b => b.value.abs > Int.MaxValue && b.value.abs <= Long.MaxValue) ||
                    valueRange.max.exists(b => b.value.abs > Int.MaxValue && b.value.abs <= Long.MaxValue)
                }

                val avroType =
                    if isInteger then if hasLargeNumbers then "long" else "int"
                    else "double"

                val contextWithFriction =
                    if constraints.value.isDefined then
                        context.addFriction(
                            FrictionType.Loss,
                            "Numeric value constraints cannot be represented in Avro"
                        )
                    else context

                (Json.Str(avroType), contextWithFriction)

            case BinaryValue(constraints) =>
                val contextWithFriction =
                    if !constraints.isEmpty then
                        context.addFriction(
                            FrictionType.Loss,
                            s"Binary constraints cannot be represented in Avro"
                        )
                    else context
                (Json.Str("bytes"), contextWithFriction)

            case TimeValue(constraints*) =>
                // Map time formats to Avro logical types
                val constraint = constraints.headOption.getOrElse(TimeConstraint.NamedFormat.ISO8601)
                constraint match
                    case TimeConstraint.NamedFormat.ISO8601 | TimeConstraint.NamedFormat.RFC3339 | TimeConstraint.NamedFormat.ISO8601_DateTime =>
                        val contextWithFriction = context.addFriction(
                            FrictionType.Approximation,
                            "ISO8601/RFC3339 timestamp approximated as Avro timestamp-millis logical type"
                        )
                        (
                            Json.Obj(
                                "type"        -> Json.Str("long"),
                                "logicalType" -> Json.Str("timestamp-millis")
                            ),
                            contextWithFriction
                        )
                    case TimeConstraint.NamedFormat.ISO8601_Date                                                                               =>
                        (
                            Json.Obj(
                                "type"        -> Json.Str("int"),
                                "logicalType" -> Json.Str("date")
                            ),
                            context
                        )
                    case TimeConstraint.NamedFormat.ISO8601_Time                                                                               =>
                        val contextWithFriction = context.addFriction(
                            FrictionType.Approximation,
                            "ISO8601 time approximated as Avro time-millis logical type"
                        )
                        (
                            Json.Obj(
                                "type"        -> Json.Str("int"),
                                "logicalType" -> Json.Str("time-millis")
                            ),
                            contextWithFriction
                        )
                    case TimeConstraint.CustomPattern(pattern)                                                                                 =>
                        val contextWithFriction = context.addFriction(
                            FrictionType.Loss,
                            s"Custom time pattern '$pattern' cannot be represented in Avro, using string"
                        )
                        (Json.Str("string"), contextWithFriction)

            case ListOfValues(itemSchema, constraints) =>
                val (itemJson, itemContext) = translateSchema(itemSchema, context.withPath(s"${ context.path }[]"))
                val finalContext            =
                    if !constraints.isEmpty then
                        itemContext.addFriction(
                            FrictionType.Loss,
                            s"List constraints cannot be represented in Avro"
                        )
                    else itemContext
                (Json.Obj("type" -> Json.Str("array"), "items" -> itemJson), finalContext)

            case MapValue(valueSchema) =>
                val (valueJson, valueContext) = translateSchema(valueSchema, context.withPath(s"${ context.path }{}"))
                (Json.Obj("type" -> Json.Str("map"), "values" -> valueJson), valueContext)

            case ObjectValue(fields) =>
                val recordName     = nameHint.getOrElse("Record")
                var currentContext = context
                val avroFields     = fields.toSeq.sortBy(_._1.label).map { case (label, fieldSchema) =>
                    val fieldName = label.label
                    val fieldPath = if context.path.isEmpty then fieldName else s"${ context.path }.$fieldName"

                    label match
                        case MandatoryLabel(_) =>
                            val (fieldJson, newContext) = translateSchema(fieldSchema, currentContext.withPath(fieldPath))
                            currentContext = newContext
                            Json.Obj(
                                "name" -> Json.Str(fieldName),
                                "type" -> fieldJson
                            )
                        case OptionalLabel(_)  =>
                            val (fieldJson, newContext)    = translateSchema(fieldSchema, currentContext.withPath(fieldPath))
                            currentContext = newContext
                            Json.Obj(
                                "name"    -> Json.Str(fieldName),
                                "type"    -> Json.Arr(Json.Str("null"), fieldJson),
                                "default" -> Json.Null
                            )
                }
                (
                    Json.Obj(
                        "type"   -> Json.Str("record"),
                        "name"   -> Json.Str(recordName),
                        "fields" -> Json.Arr(avroFields*)
                    ),
                    currentContext
                )

            case TupleValue(items*) =>
                // Avro doesn't have tuples - use record with indexed field names
                var currentContext = context.addFriction(
                    FrictionType.Approximation,
                    "Tuple converted to Avro record with field names 'field0', 'field1', etc."
                )
                val recordName     = nameHint.getOrElse("Tuple")
                val avroFields     = items.zipWithIndex.map { case (itemSchema, idx) =>
                    val fieldName               = s"field$idx"
                    val fieldPath               = if context.path.isEmpty then fieldName else s"${ context.path }.$fieldName"
                    val (fieldJson, newContext) = translateSchema(itemSchema, currentContext.withPath(fieldPath))
                    currentContext = newContext
                    Json.Obj(
                        "name" -> Json.Str(fieldName),
                        "type" -> fieldJson
                    )
                }
                (
                    Json.Obj(
                        "type"   -> Json.Str("record"),
                        "name"   -> Json.Str(recordName),
                        "fields" -> Json.Arr(avroFields*)
                    ),
                    currentContext
                )

            case AlternativeValues(alternatives*) =>
                var currentContext = context
                val unionTypes     = alternatives.map { alt =>
                    val (altJson, newContext) = translateSchema(alt, currentContext)
                    currentContext = newContext
                    altJson
                }
                (Json.Arr(unionTypes*), currentContext)

            case EnumValues(values*) =>
                val enumName = nameHint.getOrElse("Enum")
                (
                    Json.Obj(
                        "type"    -> Json.Str("enum"),
                        "name"    -> Json.Str(enumName),
                        "symbols" -> Json.Arr(values.map(Json.Str(_))*)
                    ),
                    context
                )

            case GivenTextValue(text) =>
                // Avro doesn't have constant types - use enum with single value
                val (enumName, updatedContext) = context.nextName("ConstEnum")
                val finalContext               = updatedContext.addFriction(
                    FrictionType.Approximation,
                    s"Constant value '$text' represented as enum with single symbol"
                )
                (
                    Json.Obj(
                        "type"    -> Json.Str("enum"),
                        "name"    -> Json.Str(enumName),
                        "symbols" -> Json.Arr(Json.Str(text))
                    ),
                    finalContext
                )

            case Documented(doc, innerSchema) =>
                val (json, newContext) = translateSchema(innerSchema, context, nameHint)
                json match
                    case obj: Json.Obj =>
                        (obj.add("doc", Json.Str(doc.getOrElse(""))), newContext)
                    case other         =>
                        val finalContext = newContext.addFriction(
                            FrictionType.Loss,
                            s"Documentation comment cannot be attached to non-record type, lost: ${ doc.getOrElse("") }"
                        )
                        (other, finalContext)

            case Deprecated(innerSchema) =>
                val finalContext = context.addFriction(
                    FrictionType.Loss,
                    "Deprecated annotation not representable in Avro"
                )
                translateSchema(innerSchema, finalContext, nameHint)

            case NamedValueReference(name) =>
                (Json.Str(name), context)

            case ScopedReference(name, namespace) =>
                val fullName = if namespace.isEmpty then name else s"$namespace.$name"
                (Json.Str(fullName), context)

            case Fail() =>
                val finalContext = context.addFriction(
                    FrictionType.Loss,
                    "Fail type not representable in Avro, using string"
                )
                (Json.Str("string"), finalContext)

            case ImportStatement(namespace, path) =>
                val finalContext = context.addFriction(
                    FrictionType.Loss,
                    "Import statements not supported in Avro translation"
                )
                (Json.Str("string"), finalContext)
