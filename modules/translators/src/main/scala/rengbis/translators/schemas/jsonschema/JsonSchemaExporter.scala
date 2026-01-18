package rengbis.translators.schemas.jsonschema

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.json.ast.Json

/** Exports ReNGBis schemas to JSON Schema format.
  *
  * This exporter translates ReNGBis schemas to JSON Schema Draft 2020-12. Since JSON Schema has different semantics and capabilities, some features may not translate perfectly. The FrictionReport tracks any translation issues.
  */
object JsonSchemaExporter:

    /** Exports a ReNGBis schema to JSON Schema.
      *
      * @param schema
      *   The ReNGBis schema to toJsonSchema
      * @return
      *   A tuple of (JSON Schema as Json.Obj, FrictionReport)
      */
    def toJsonSchema(schema: Schema): (Json.Obj, FrictionReport) =
        val context = TranslationContext()
        val result  = translateSchema(schema, context)
        (result._1, result._2)

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport()
    ):
        def atPath(newPath: String): TranslationContext =
            copy(path = if newPath.startsWith("$") then newPath else s"$path/$newPath")

        def addLoss(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addLoss(path, message, suggestion))

        def addApproximation(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addApproximation(path, message, suggestion))

        def addExtension(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addExtension(path, message, suggestion))

    private def translateSchema(schema: Schema, context: TranslationContext): (Json.Obj, FrictionReport) =
        schema match
            // Wrapper schemas
            case Documented(doc, innerSchema) =>
                val (json, report) = translateSchema(innerSchema, context)
                val withDesc       = doc match
                    case Some(d) => json.add("description", Json.Str(d))
                    case None    => json
                (withDesc, report)

            case Deprecated(innerSchema) =>
                val (json, report) = translateSchema(innerSchema, context)
                (json.add("deprecated", Json.Bool(true)), report)

            // Basic types
            case TextValue(constraints, default) =>
                translateTextValue(constraints, default, context)

            case GivenTextValue(value) =>
                (Json.Obj("const" -> Json.Str(value)), context.report)

            case NumericValue(constraints, default) =>
                translateNumericValue(constraints, default, context)

            case TimeValue(constraints @ _*) =>
                translateTimeValue(constraints, context)

            case BooleanValue() =>
                (Json.Obj("type" -> Json.Str("boolean")), context.report)

            case AnyValue() =>
                (Json.Obj(), context.report) // Empty schema means "anything"

            case Fail() =>
                (Json.Obj("not" -> Json.Obj()), context.report) // Fail means reject everything

            case BinaryValue(constraints @ _*) =>
                translateBinaryValue(constraints, context)

            // Container types
            case ListOfValues(elementSchema, constraints @ _*) =>
                translateListOfValues(elementSchema, constraints, context)

            case TupleValue(elementSchemas @ _*) =>
                translateTupleValue(elementSchemas, context)

            case ObjectValue(obj) =>
                translateObjectValue(obj, context)

            case MapValue(valueSchema) =>
                translateMapValue(valueSchema, context)

            // Enum
            case EnumValues(values @ _*) =>
                (Json.Obj("enum" -> Json.Arr(values.map(Json.Str(_))*)), context.report)

            // References
            case NamedValueReference(name) =>
                (Json.Obj("$ref" -> Json.Str(s"#/$$defs/$name")), context.report)

            case ScopedReference(namespace, name) =>
                val refName = if name.isEmpty then namespace else s"$namespace.$name"
                (Json.Obj("$ref" -> Json.Str(s"#/$$defs/$refName")), context.report)

            // Combinators
            case AlternativeValues(options @ _*) =>
                translateAlternativeValues(options, context)

            // Import statements don't translate directly
            case ImportStatement(namespace, path) =>
                val ctx = context.addLoss(
                    s"Import statement for namespace '$namespace' from '$path' cannot be represented in JSON Schema",
                    Some("Inline the imported definitions")
                )
                (Json.Obj(), ctx.report)

    private def translateTextValue(constraints: Seq[TextConstraint.Constraint], default: Option[String], context: TranslationContext): (Json.Obj, FrictionReport) =
        var baseObj    = Json.Obj("type" -> Json.Str("string"))
        var currentCtx = context

        constraints.foreach:
            case TextConstraint.Regex(pattern) =>
                baseObj = baseObj.add("pattern", Json.Str(pattern))

            case TextConstraint.Format(format) =>
                // Map common formats to JSON Schema formats
                val jsonSchemaFormat = format.toLowerCase match
                    case "email"    => Some("email")
                    case "uri"      => Some("uri")
                    case "uuid"     => Some("uuid")
                    case "ipv4"     => Some("ipv4")
                    case "ipv6"     => Some("ipv6")
                    case "hostname" => Some("hostname")
                    case _          =>
                        currentCtx = currentCtx.addExtension(
                            s"Custom text format '$format' requires JSON Schema extension",
                            Some("Consider using pattern constraint instead")
                        )
                        None

                jsonSchemaFormat.foreach(f => baseObj = baseObj.add("format", Json.Str(f)))

            case TextConstraint.Size(bound) =>
                bound.op match
                    case BoundOp.Exact        =>
                        baseObj = baseObj.add("minLength", Json.Num(bound.value))
                        baseObj = baseObj.add("maxLength", Json.Num(bound.value))
                    case BoundOp.MinInclusive =>
                        baseObj = baseObj.add("minLength", Json.Num(bound.value))
                    case BoundOp.MinExclusive =>
                        baseObj = baseObj.add("minLength", Json.Num(bound.value + 1))
                    case BoundOp.MaxInclusive =>
                        baseObj = baseObj.add("maxLength", Json.Num(bound.value))
                    case BoundOp.MaxExclusive =>
                        baseObj = baseObj.add("maxLength", Json.Num(bound.value - 1))

        default.foreach(d => baseObj = baseObj.add("default", Json.Str(d)))

        (baseObj, currentCtx.report)

    private def translateNumericValue(constraints: Seq[NumericConstraint.Constraint], default: Option[BigDecimal], context: TranslationContext): (Json.Obj, FrictionReport) =
        var baseObj    = Json.Obj("type" -> Json.Str("number"))
        var currentCtx = context
        var isInteger  = false

        constraints.foreach:
            case NumericConstraint.Integer =>
                baseObj = Json.Obj("type" -> Json.Str("integer"))
                isInteger = true

            case NumericConstraint.Value(bound) =>
                bound.op match
                    case BoundOp.Exact        =>
                        baseObj = baseObj.add("const", Json.Num(bound.value))
                    case BoundOp.MinInclusive =>
                        baseObj = baseObj.add("minimum", Json.Num(bound.value))
                    case BoundOp.MinExclusive =>
                        baseObj = baseObj.add("exclusiveMinimum", Json.Num(bound.value))
                    case BoundOp.MaxInclusive =>
                        baseObj = baseObj.add("maximum", Json.Num(bound.value))
                    case BoundOp.MaxExclusive =>
                        baseObj = baseObj.add("exclusiveMaximum", Json.Num(bound.value))

        default.foreach(d => baseObj = baseObj.add("default", Json.Num(d)))

        (baseObj, currentCtx.report)

    private def translateTimeValue(constraints: Seq[TimeConstraint.Constraint], context: TranslationContext): (Json.Obj, FrictionReport) =
        var baseObj    = Json.Obj("type" -> Json.Str("string"))
        var currentCtx = context

        constraints.foreach { case fc: TimeConstraint.FormatConstraint =>
            fc match
                case TimeConstraint.NamedFormat.ISO8601 | TimeConstraint.NamedFormat.ISO8601_DateTime =>
                    baseObj = baseObj.add("format", Json.Str("date-time"))

                case TimeConstraint.NamedFormat.ISO8601_Date =>
                    baseObj = baseObj.add("format", Json.Str("date"))

                case TimeConstraint.NamedFormat.ISO8601_Time =>
                    baseObj = baseObj.add("format", Json.Str("time"))

                case TimeConstraint.NamedFormat.RFC3339 =>
                    baseObj = baseObj.add("format", Json.Str("date-time"))

                case TimeConstraint.CustomPattern(pattern) =>
                    currentCtx = currentCtx.addLoss(
                        s"Custom time pattern '$pattern' cannot be represented in JSON Schema",
                        Some("JSON Schema only supports standard date-time formats")
                    )
                    baseObj = baseObj.add("format", Json.Str("date-time"))
        }

        (baseObj, currentCtx.report)

    private def translateBinaryValue(constraints: Seq[BinaryConstraint.Constraint], context: TranslationContext): (Json.Obj, FrictionReport) =
        var baseObj    = Json.Obj("type" -> Json.Str("string"))
        var currentCtx = context

        constraints.foreach:
            case BinaryConstraint.Encoding(encoder) =>
                val format = encoder match
                    case BinaryConstraint.BinaryToTextEncoder.base64 => "byte" // JSON Schema uses "byte" for base64
                    case _                                           =>
                        currentCtx = currentCtx.addApproximation(
                            s"Binary encoding '${ encoder.code }' not natively supported in JSON Schema, using base64",
                            Some("Consider using 'byte' format (base64)")
                        )
                        "byte"
                baseObj = baseObj.add("format", Json.Str(format))

            case BinaryConstraint.Size(bound, unit) =>
                // JSON Schema doesn't have direct size constraints for binary data
                currentCtx = currentCtx.addLoss(
                    s"Binary size constraint (${ bound.value } ${ unit.symbol }) cannot be enforced in JSON Schema",
                    Some("Add description or use custom validation")
                )

        (baseObj, currentCtx.report)

    private def translateListOfValues(elementSchema: Schema, constraints: Seq[ListConstraint.Constraint], context: TranslationContext): (Json.Obj, FrictionReport) =
        val (itemsJson, itemsReport) = translateSchema(elementSchema, context.atPath("items"))
        var baseObj                  = Json.Obj("type" -> Json.Str("array"), "items" -> itemsJson)
        var currentCtx               = context.copy(report = itemsReport)

        constraints.foreach:
            case ListConstraint.Size(bound) =>
                bound.op match
                    case BoundOp.Exact        =>
                        baseObj = baseObj.add("minItems", Json.Num(bound.value))
                        baseObj = baseObj.add("maxItems", Json.Num(bound.value))
                    case BoundOp.MinInclusive =>
                        baseObj = baseObj.add("minItems", Json.Num(bound.value))
                    case BoundOp.MinExclusive =>
                        baseObj = baseObj.add("minItems", Json.Num(bound.value + 1))
                    case BoundOp.MaxInclusive =>
                        baseObj = baseObj.add("maxItems", Json.Num(bound.value))
                    case BoundOp.MaxExclusive =>
                        baseObj = baseObj.add("maxItems", Json.Num(bound.value - 1))

            case ListConstraint.Unique =>
                baseObj = baseObj.add("uniqueItems", Json.Bool(true))

            case ListConstraint.UniqueByFields(fields) =>
                currentCtx = currentCtx.addLoss(
                    s"UniqueByFields constraint (fields: ${ fields.mkString(", ") }) cannot be enforced in JSON Schema",
                    Some("Use custom validation or add description")
                )

        (baseObj, currentCtx.report)

    private def translateTupleValue(elementSchemas: Seq[Schema], context: TranslationContext): (Json.Obj, FrictionReport) =
        var currentReport = context.report
        val prefixItems   = elementSchemas.zipWithIndex.map { case (schema, idx) =>
            val (json, report) = translateSchema(schema, context.atPath(s"prefixItems[$idx]"))
            currentReport = currentReport.merge(report)
            json
        }
        (
            Json.Obj(
                "type"        -> Json.Str("array"),
                "prefixItems" -> Json.Arr(prefixItems*),
                "items"       -> Json.Bool(false)
            ),
            currentReport
        )

    private def translateObjectValue(obj: Map[ObjectLabel, Schema], context: TranslationContext): (Json.Obj, FrictionReport) =
        var currentReport    = context.report
        val properties       = obj.map { (label, schema) =>
            val (json, report) = translateSchema(schema, context.atPath(s"properties/${ label.label }"))
            currentReport = currentReport.merge(report)
            label.label -> json
        }
        val requiredFields   = obj.keys.collect { case MandatoryLabel(name) => name }.toSeq
        val baseObj          = Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj(properties.toSeq*))
        val withRequired     =
            if requiredFields.nonEmpty then baseObj.add("required", Json.Arr(requiredFields.map(Json.Str(_))*))
            else baseObj
        val withNoAdditional = withRequired.add("additionalProperties", Json.Bool(false))
        (withNoAdditional, currentReport)

    private def translateMapValue(valueSchema: Schema, context: TranslationContext): (Json.Obj, FrictionReport) =
        val (valueJson, report) = translateSchema(valueSchema, context.atPath("additionalProperties"))
        (
            Json.Obj(
                "type"                 -> Json.Str("object"),
                "additionalProperties" -> valueJson
            ),
            report
        )

    private def translateAlternativeValues(options: Seq[Schema], context: TranslationContext): (Json.Obj, FrictionReport) =
        var currentReport = context.report
        val anyOfSchemas  = options.zipWithIndex.map { case (schema, idx) =>
            val (json, report) = translateSchema(schema, context.atPath(s"anyOf[$idx]"))
            currentReport = currentReport.merge(report)
            json
        }
        (Json.Obj("anyOf" -> Json.Arr(anyOfSchemas*)), currentReport)
